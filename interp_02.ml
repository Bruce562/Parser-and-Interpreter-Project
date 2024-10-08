(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_lower_case c = 'a' <= c && c <= 'z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind
let ( let* ) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* REQUIRED TYPES *)

type ident = string

type const
  = Num of int
  | Bool of bool

type command
  = Push of const | Trace
  | Add | Mul | Div
  | And | Or | Not | Lt | Eq
  | If of program * program
  | While of program * program
  | Bind of ident | Fetch of ident
  | Fun of program | Call | Return
  | Debug of string

and program = command list

and bindings = (ident * value) list

and value
  = Const of const
  | Clos of
      { def_id : int
      ; captured : bindings
      ; prog : program
      }

type record =
  { id : int
  ; local : bindings
  ; called_def_id : int
  ; return_prog : program
  }

type stack = value list
type trace = string list
type env
  = Global of bindings
  | Local of record * env

(* get the id of the topmost record *)
let local_id = function
  | Global _ -> 0
  | Local (r, _) -> r.id

(* convert a value to a string *)
let to_string v =
  match v with
  | Const (Bool true) -> "True"
  | Const (Bool false) -> "False"
  | Const (Num n) -> string_of_int n
  | Clos _ -> "<Closure>"

(* PARSING *)

let parse_ident =
  map2
    (fun c cs -> implode (c :: cs))
    (satisfy is_lower_case)
    (many (satisfy (fun c -> is_lower_case c || is_digit c || c = '_')))

let parse_int =
  let mk_int sign cs =
    let abs = int_of_string (implode cs) in
    if Option.is_none sign
    then abs
    else -abs
  in
  map2
    mk_int
    (optional (char '-'))
    (many1 (satisfy is_digit))

let parse_bool =
  (str "True" >| true) <|> (str "False" >| false)

let parse_comment =
  char '(' >> many (satisfy ((<>) ')')) >> char ')' >| ()

let parse_debug =
  char '"' >> many (satisfy ((<>) '"')) << char '"' >|= implode

let ws = many (ws >> parse_comment) >> ws
let keyword w = str w << ws

let endOfProg cs =
  let rec go ls acc level=
    match ls with
      | [] -> ([], [])
      | '?'::rest -> go rest (acc@['?']) (level+2)
      | 'W'::rest -> go rest (acc@['W']) (level+2)
      | ':'::rest -> go rest (acc@[':']) (level+1) 
      | ';'::rest when level = 1 -> (acc, rest)
      | ';'::rest -> go rest (acc@[';']) (level-1)
      | x::rest -> go rest (acc@[x]) level
  in go cs [] 1

let parse_keyword = [str "&&"; str "||"; str "|>"; str "True"; str "False"; str "Return"; str "While"]
let parse_whileif cs = 
  match endOfProg cs with 
  | ([],[]) -> (([],[]),[])
  | (prog1, rest) -> 
    (match endOfProg rest with
    | ([],[]) -> (([],[]),[])
    | (prog2, rest) -> ((prog1, prog2), rest))


let rec parse_prog s = 
  let rec go cs acc = 
    match (ws cs) with
    | Some (_, cs) -> 
      (match cs with 
      | [] -> acc
      | x::xs -> 
        if x = '+' then (*Add cmd*)
          go xs (acc@[Add]) 
        else if x = '-' then (*Push cmd for negative numbers*)
          (match xs with
          | [] -> [] (*- sign not followed by an int*)
          | _::_ -> 
            (match parse_int xs with
            | Some (i, rest) -> go (rest) (acc@[Push (Num (-i))])
            | None -> []
            )
          )
        else if is_digit x then (*Push cmd for positive numbers*)
          match parse_int cs with 
          | Some (i, rest) -> go rest (acc@[Push (Num i)])
          | None -> []
        else if x = '*' then (*Mul cmd*)
          go xs (acc@[Mul]) 
        else if x = '/' then (*Div cmd*)
          go xs (acc@[Div]) 
        else if x = '<' then (*Lt cmd*)
          go xs (acc@[Lt]) 
        else if x = '=' then (*Eq cmd*)
          go xs (acc@[Eq]) 
        else if x = '.' then (*Trace cmd*)
          go xs (acc@[Trace]) 
        else if is_lower_case x then (*Fetch ident cmd*)
          match parse_ident cs with
          | Some (id, rest) -> go rest (acc@[Fetch id])
          | None -> []
        else if x = '~' then  (*Not cmd*)
          go xs (acc@[Not])
        else if x = '"' then (*Debug cmd*)
          match parse_debug cs with 
          | Some (s, rest) -> go rest (acc@[Debug s])
          | None -> []
        else if x = '(' then (*Comment cmd*)
          match parse_comment cs with 
          | Some (_, rest) -> go rest acc 
          | None -> []
        else if x = '#' then  (*Call cmd*)
          go xs (acc@[Call])
        else if x = ':' then (*Fun cmd*)
          match endOfProg xs with 
          | ([],[]) -> []
          | (func, rest) ->
            (match rest with 
            | [] -> []
            | x::rest -> go rest (acc@[Fun (go func [])]))
        else if x = '?' then (*If-else cmd*)
          match parse_whileif xs with
          | (([],[]), []) -> []
          | ((prog1, prog2), rest)-> go rest (acc@[If ((go prog1 []),(go prog2 []))])
        else
          (match choice parse_keyword cs with 
          | Some (kw, rest) -> 
              if kw = "&&" then (*And cmd*)
                go rest (acc@[And])
              else if kw = "||" then
                go rest (acc@[Or]) (*Or cmd*)
              else if kw = "|>" then (*Bind cmd*)
                (match ws rest with
                | Some (_,rest) -> 
                  (match parse_ident rest with (*Grabs id for bind*)
                  | Some (id, rest) -> go rest (acc@[Bind id])
                  | None -> [])
                | None -> [])
              else if kw = "True" then (*Push true cmd*)
                go rest (acc@[Push (Bool true)])
              else if kw = "False" then (*Push false*)
                go rest (acc@[Push (Bool false)])
              else if kw = "While" then (*While cmd*)
                match parse_whileif rest with
                | (([],[]), []) -> []
                | ((prog1, prog2), rest)-> go rest (acc@[While (go prog1 [], go prog2 [])])
              else if kw = "Return" then (*Return cmd*)
                go rest (acc@[Return]) 
              else (*Parse error*)
                []
          | None -> []
          )
        ) 
    | None -> []
  in 
  match (go (explode s) []) with
  | [] when s <> "" -> None 
  | cmdL -> Some (cmdL)


let testParser = 
  "(fact): |> n
  n |> i
   1 |> out
  While 0 i = ~ ;
    out i * |> out
     -1  i + |> i
  ;
  out Return
; |> fact

\"\"
\"factorial of 6:\"
6  fact# .

"

        
(* FETCHING AND UPDATING *)

(* fetch the value of `x` in the environment `e` *)
let rec loopBindings b x = (*Loops through bindings and returns a value*)
  match b with 
  | [] -> None
  | bind::rest ->
    match bind with 
    | (id, v) ->
      if id=x then 
        Some (v)
      else
        loopBindings rest x

let rec fetch_env e x = 
  match e with 
  | Global b -> (*Global scope*)
    (match (loopBindings b x) with 
    | None -> None 
    | Some v -> Some v)
  | Local (r, _) -> (*Local scope*)
    match (loopBindings r.local x) with 
    | Some v -> Some v
    | None ->
      let rec findCallee e = (*Finding the callee identifier*)
        (match e with
        | Global _ -> fetch_env e x (*In global - search bindings*)
        | Local (nr, ne) ->
          if nr.id = r.caller_def_id then
            fetch_env ne x 
          else
            findCallee ne)
      in findCallee e

let rec updateBindings b x v acc = (*Loops through bindings and updates*)
  match b with
  | [] -> [(x, v)]@acc
  | (id, curV)::rest -> 
    (if id = x then 
      (x, v)::acc@rest (*Doesn't allow shadowing. Updates id instead of adding id with new val*)
    else
      updateBindings rest x v ((id, curV)::acc))

let update_env e x v = 
  match e with 
  | Global b -> 
    (match (updateBindings b x v []) with
    | newBindings -> Some (Global newBindings)
    )
  | Local (r, ne) -> 
    (match (updateBindings r.local x v []) with 
    | newBindings -> let newE = {r with local=newBindings} in Some (Local (newE, ne)))
(* EVALUTION *)

(* make the panic configuration given a configuration *)
let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []

let eval_step (c : stack * env * trace * program) =
  match c with
  (* Push *)
  | s, e, t, Push c :: p -> Const c :: s, e, t, p
  | s, e, t, Fun q :: p -> Clos {def_id=(local_id e); captured=[]; prog=q}:: s, e, t, p
  (* Trace *)
  | v :: s, e, t, Trace :: p -> s, e, to_string v :: t, p
  | [], _, _, Trace :: _ -> panic c "stack underflow (. on empty)"
  (* Add *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Add :: p -> Const (Num (m + n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Add :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Add :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Add :: _ -> panic c "stack underflow (+ on empty)"
  (* Mul *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Mul :: p -> Const (Num (m * n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Mul :: _ -> panic c "type error (* on non-integers)"
  | _ :: [], _, _, Mul :: _ -> panic c "stack underflow (* on single)"
  | [], _, _, Mul :: _ -> panic c "stack underflow (* on empty)"
  (* Div *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p -> 
    (match n with
    | 0 -> panic c "cannot divide by 0"
    | _ -> Const (Num (m / n)) :: s, e, t, p)
  | _ :: _ :: _, _, _, Div :: _ -> panic c "type error (* on non-integers)"
  | _ :: [], _, _, Div :: _ -> panic c "stack underflow (* on single)"
  | [], _, _, Div :: _ -> panic c "stack underflow (* on empty)"
  (* And *)
  | Const (Bool m)::Const (Bool n)::s, e, t, And::p -> Const (Bool (m&&n))::s, e, t, p 
  | _ :: _ :: _, _, _, And :: _ -> panic c "type error (* on non-bools)"
  | _ :: [], _, _, And :: _ -> panic c "stack underflow (* on single)"
  | [], _, _, And :: _ -> panic c "stack underflow (* on empty)"
  (* Or *)
  | Const (Bool m)::Const (Bool n)::s, e, t, Or::p -> Const (Bool (m||n))::s, e, t, p 
  | _ :: _ :: _, _, _, Or :: _ -> panic c "type error (* on non-bools)"
  | _ :: [], _, _, Or :: _ -> panic c "stack underflow (* on single)"
  | [], _, _, Or :: _ -> panic c "stack underflow (* on empty)"
  (* Not *)
  | Const (Bool m)::s, e, t, Not::p -> Const (Bool (not m))::s, e, t, p 
  | _ :: _ , _, _, Not :: _ -> panic c "type error (* on non-bools)"
  | [], _, _,  Not :: _ -> panic c "stack underflow (* on empty)"
  (* Lt *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Lt :: p -> 
    if m<n then
      Const (Bool true) :: s, e, t, p
    else 
      Const (Bool false) :: s, e, t, p
  | _ :: _ :: _, _, _, Lt :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Lt :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Lt :: _ -> panic c "stack underflow (+ on empty)"
  (* Eq *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Eq :: p ->
      if m=n then
        Const (Bool true) :: s, e, t, p
      else 
        Const (Bool false) :: s, e, t, p
  | _ :: _ :: _, _, _, Eq :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Eq :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Eq :: _ -> panic c "stack underflow (+ on empty)"
  (* If *)
  | Const (Bool tf) :: s, e, t, If (prog1, prog2)::p -> 
    if tf then 
      s,e,t,(prog1@p)
    else
      s,e,t,(prog2@p)
  | _ :: _ , _, _, If (_,_):: _ -> panic c "type error (+ on non-bools)"
  | [], _, _, If (_,_):: _ -> panic c "stack underflow (+ on empty)"
  (* While *)
  | s, e, t, While (prog1, prog2)::p ->
      s, e, t, (prog1@[If ((prog2@[While (prog1, prog2)]@p), p)])
  (* Call *)
  | Clos r::s, e, t, Call::p -> 
    (let newEnv = {id=(r.def_id+1); local=r.captured; caller_def_id=r.def_id; return_prog=p}
      in s, Local (newEnv, e), t, r.prog)
  | _::s, e, t, Call::p -> panic c "type error (+ on non-closure)"
  | [], e, t, Call::p -> panic c "stack underflow (+ on empty)"
  (* Bind *)
  | v::s, e, t, Bind x::p -> 
    (match (update_env e x v) with 
      | Some newE -> s, newE, t, p
      | None -> assert false)
  | [], e, t, Bind x::p -> panic c "stack underflow (+ on empty)"
  (* Fetch *)
  | s, e, t, Fetch id::p -> 
    (match fetch_env e id with 
    | Some v -> v::s, e, t, p
    | None -> panic c "id is unbound")
  (* Return *)
  | Clos v::[], Local (r, ne), t, Return::p ->
    if v.def_id=(r.id-1) then
      Clos v::[], ne, t, r.return_prog
    else 
      Clos v::[], ne, t, r.return_prog
  | Const x::[], Local (r, ne), t, Return::p -> Const x::[], ne, t, r.return_prog
  | [], Local (r, ne), t, Return::p -> [], ne, t, r.return_prog
  | [], Local (r, ne), t, [] -> [], ne, t, r.return_prog
  | _, Global _, _, Return::p -> panic c "cannot return in global"
  | _::_, _, _, [] -> panic c "stack not empty"
  | _::_::_, _, _, Return::p -> panic c "stack not empty"
  (* Debug *)
  | s, e, t, Debug st::p -> s, e, st::t, p
  | _ -> assert false

let rec eval c =
  match c with
  | (_, Global _, t, []) -> t
  | _ -> eval (eval_step c)

let rec eval_prog p = eval ([], Global [], [], p)
let interp s = Option.map eval_prog (parse_prog s)

let fact = 
"(fact): |> n
(if) 0 n < ?
  -1 Return ;
(else if) n 0 = ?
  1 Return ;
(else)
  n -1 + fact# n *
  Return ;
;
; |> fact

\"factorial of 5:\"
5 fact# .

(fact): |> n
n |> i
 1 |> out
While 0 i = ~ ;
  out i * |> out
   -1  i + |> i
;
out Return
; |> fact


\"factorial of 6:\"
6  fact# ."


let fib = 
"(fib): |> n
(if)
  n 0 =
  n 1 =
  || ?
(then)
  n Return ;
(else)
  -1 n + fib # |> x
  -2 n + fib # |> y
  x y + Return ;
; |> fib

\"10th Fibonacci Number:\"
10 fib # .

(print_fib): |> n
0 |> count
While count n = ~ ;
  count fib # .
  count 1 + |> count
;
; |> print_fib


\"The first 10 Fibonacci Numbers:\"
10 print_fib #
"

let prime = 
"(sqrt): |> n
(if) 0 n < ? Return ; ;
(if) 0 n = ? 0 Return ; ;
0 |> i
While n i < ;
  (if) i i * n < ?
    i -1 + Return ;
  (else)
    i 1 + |> i
  ;
;
; |> sqrt

(is_prime): |> n
(if) 2 n < ? False Return ; ;
2 |> i
n sqrt # |> s
While s i < ;
  (if) i n / i * n = ?
    False Return ;
  (else)
    i 1 + |> i
  ;
;
True Return
; |> is_prime

15261281789861 is_prime # ."

let ex1 = 
"(f):
0 |> x
(g):
  (h):
    x .
  ;
  Return
; #
Return
; |> f

\"Calling the output of f (should print 0):\"
f # |> g
g #
"
(* MAIN *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)

let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

 (*let _ = main () *)

(* END OF FILE *)
