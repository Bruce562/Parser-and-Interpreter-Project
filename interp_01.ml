(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

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

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *done**)
  | Swap                   (* swap *done**)
  | Dup                    (* dup *done**)
  | Trace                  (* . *done**)
  | Add                    (* + *done**)
  | Sub                    (* - *done**)
  | Mul                    (* * *done**)
  | Div                    (* / *done**)
  | Lt                     (* < *done**)
  | Eq                     (* = *done**)
  | Bind of ident          (* |> ID *done**)
  | Call of ident          (* # ID *done**)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *done**)
  | Num of int             (* num *done**) 
and program = command list

let parse_ident = 
  many1 (satisfy is_upper_case) >|= implode

(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
(*let rec parse_com () =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (keyword "def" >> parse_ident << ws)
      (parse_prog_rec () << char ';')
  in parse_def <|> fail (* TODO *)
and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)*)

let parse_keyword = [str "drop"; str "swap"; str "dup"; str "|>"; str "def"]
  
let endOfDef cs =
  let rec go ls acc level=
    match ls with
      | [] -> ([], [])
      | ';'::rest when level = 1 -> (acc, ls)
      | '?'::rest -> go rest (acc@['?']) (level+1)
      | ';'::rest -> go rest (acc@[';']) (level-1)
      | x::y::z::rest when x = 'd' && y = 'e' && z = 'f' -> go rest (acc@[x;y;z]) (level+1)
      | x::rest -> go rest (acc@[x]) level
  in go cs [] 1

let rec parse_prog s = 
  let rec go cs acc = 
    match (ws cs) with
    | Some (_, cs) ->
      (match cs with
      | [] -> acc 
      | x::xs -> 
          if is_digit x then (*Retrieves the number. Works*)
            match many (satisfy is_digit) cs with
            | Some (d, rest) -> go (rest) (acc@[Num (int_of_string (implode d))])
            | None -> assert false
          else if is_upper_case x then (*Retrieves an ID. Works*)
            match parse_ident cs with 
            | Some (id, rest) -> go rest (acc@[Ident id])
            | None -> assert false
          else if x = '.' then 
            go xs (acc@[Trace]) 
          else if x = '+' then 
            go xs (acc@[Add]) 
          else if x = '-' then 
            go xs (acc@[Sub]) 
          else if x = '*' then 
            go xs (acc@[Mul]) 
          else if x = '/' then 
            go xs (acc@[Div]) 
          else if x = '<' then 
            go xs (acc@[Lt]) 
          else if x = '=' then 
            go xs (acc@[Eq]) 
          else if x = '#' then (*Call commands checks for # then grabs the ident*)
            match ws xs with
            | Some (_,rest) -> 
              (match parse_ident rest with 
              | Some (id, rest) -> go rest (acc@[Call id])
              | None -> assert false
              )
            | None -> []
          else if x = '?' then 
            (match endOfDef xs with (*Finds the ; delimeter to end the def*)
              | ([],[]) -> []
              | (def, rest) ->
                (match rest with 
                | [] -> [] 
                | x::rest -> go rest (acc@[If (go def [])])) (*Runs the recursive loop with only the char list def*)
            )
          else
            (match choice parse_keyword cs with (*Uses the choice function and parse_keyword to see which keyword appears*)
            | Some (kw, rest) -> 
              if kw = "drop" then 
                go rest (acc@[Drop])
              else if kw = "swap" then 
                go rest (acc@[Swap])
              else if kw = "dup" then 
                go rest (acc@[Dup])
              else if kw = "|>" then 
                (match ws rest with
                | Some (_,rest) -> 
                  (match parse_ident rest with (*Grabs id for bind*)
                  | Some (id, rest) -> go rest (acc@[Bind id])
                  | None -> []
                  )
                | None -> [])
              else if kw = "def" then 
                (match ws rest with
                | Some (_,rest) -> 
                  (match parse_ident rest with (*Grabs id for def*)
                  | Some (id, rest) -> 
                    (match endOfDef rest with (*Finds the ; delimeter to end the def*)
                    | ([],[]) -> []
                    | (def, rest) ->
                      (match rest with
                      | [] -> []
                      | x::rest -> go rest (acc@[Def (id, go def [])])) (*Runs the recursive loop with only the char list def*)
                    )
                  | None -> []
                  )
                | None -> [])
              else
                []
            | None -> [])
             
      )
    | None -> [] 
  in 
  match (go (explode s) []) with 
  | [] -> None 
  | cmdL -> Some (cmdL)

(* A VERY SMALL TEST SET *)

let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)

let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = 
  parse_prog "def ABS
    dup 0 swap < ?
      0 -

    ;
  ;
    30 0 -
    #ABS |>X 
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)


(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env (e : env) (uid : ident) (newV : value) = 
  let rec go e acc = 
    match e with
    | [] -> [(uid, newV)]
    | (id , v)::rest -> 
      if uid = id then 
        [(uid, newV)]@acc@rest (*If id is found and is the same value, remove and add the updated id to the top of the environment*)
      else
        go rest acc@[(id, v)]
  in go e [] 

let rec fetch_env e fid = 
  match e with 
  | [] -> None
  | (id , v)::rest -> 
    if fid = id then 
      Some v 
    else 
      fetch_env rest fid

let eval_prog pl = 
  let rec rec_cmd (s : stack) (e : env) (t : trace) (pl : program) =
    (match pl with
    | [] -> t
    | cmd::otherCmds -> 
      (match cmd with
      | Drop -> 
        (match s with
        | [] -> rec_cmd s e ("panicDrop"::t) otherCmds
        | x::newS -> rec_cmd newS e t otherCmds)
      | Swap ->
        (match s with
        | [] | _::[] -> rec_cmd s e ("panicSwap"::t) otherCmds
        | x::y::newS -> rec_cmd (y::x::newS) e t otherCmds)
      | Dup ->
        (match s with
        | [] -> rec_cmd s e ("panicDup"::t) otherCmds
        | x::newS -> rec_cmd (x::x::newS) e t otherCmds)
      | Trace ->
        (match s with
        | [] -> rec_cmd s e ("panicTrace"::t) otherCmds
        | x::newS -> rec_cmd newS e (string_of_int(x)::t) otherCmds)
      | Add ->
        (match s with
        | [] | _::[] -> rec_cmd s e ("panicA"::t) otherCmds
        | x::y::newS -> rec_cmd ((x+y)::newS) e t otherCmds)
      | Sub ->
        (match s with
        | [] | _::[] -> rec_cmd s e ("panicS"::t) otherCmds
        | x::y::newS -> rec_cmd ((x-y)::newS) e t otherCmds)
      | Mul ->
        (match s with
        | [] | _::[] -> rec_cmd s e ("panicM"::t) otherCmds
        | x::y::newS -> rec_cmd ((x*y)::newS) e t otherCmds)
      | Div ->
        (match s with
        | [] | _::[] | _::0::[] -> rec_cmd s e ("panicD"::t) otherCmds
        | x::y::newS -> rec_cmd ((x*y)::newS) e t otherCmds)
      | Lt ->
        (match s with
        | [] | _::[] -> rec_cmd s e ("panicLT"::t) otherCmds
        | x::y::newS -> 
          if x < y then
            rec_cmd (1::newS) e t otherCmds
          else
            rec_cmd (0::newS) e t otherCmds)
      | Eq ->
        (match s with
        | [] | _::[] -> rec_cmd s e ("panicEQ"::t) otherCmds
        | x::y::newS -> 
          if x = y then
            rec_cmd (1::newS) e t otherCmds
          else
            rec_cmd (0::newS) e t otherCmds)
      | Bind id -> 
        (match s with 
        | [] -> rec_cmd s e ("panicBruh"::t) otherCmds
        | x::newS -> rec_cmd newS (update_env e id (Num x)) t otherCmds)
      | Call id ->
        (match (fetch_env e id) with
        | Some v -> 
          (match v with 
          | Prog pro -> rec_cmd s e t (pro@otherCmds) 
          | _ ->  rec_cmd s e ("panicHuh"::t) otherCmds)
        | _ -> rec_cmd s e ("panicHuh"::t) otherCmds)
      | If pro ->
        (match s with
        | [] -> rec_cmd s e ("panicIf"::t) otherCmds
        | 0::newS -> rec_cmd newS e t otherCmds
        | _::newS -> rec_cmd newS e t (pro@otherCmds))
      | Def (id, pro) -> rec_cmd s (update_env e id (Prog pro)) t otherCmds
      | Ident id -> 
        (match (fetch_env e id) with
        | Some v -> 
          (match v with
          | Num x -> rec_cmd (x::s) e t otherCmds
          | _ -> rec_cmd s e ("panicID"::t) otherCmds)
        | _ -> rec_cmd s e ("panicID2"::t) otherCmds)
      | Num x -> rec_cmd (x::s) e t otherCmds))
      in 
      rec_cmd [] [] [] pl
  
let interp file = 
  match (parse_prog file) with 
  | None -> None 
  | Some cmdList -> Some (eval_prog cmdList)

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)


(*let main () =
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

let _ = main ()
*)
