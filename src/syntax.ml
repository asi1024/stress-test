type unop = Neg
type binop = Plus | Minus | Mult | Div

type exp =
  | Const of int
  | Name of string * exp list
  | UnOp of unop * exp
  | BinOp of binop * exp * exp

type format =
  | Var of string * exp list
  | Loop of string * exp * exp * format * char
  | List of format list * char

let pp_exp, pp =
  let unop = function
    | Neg -> "-" in
  let binop = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/" in
  let rec exp = function
    | Const i -> string_of_int i
    | Name (name, []) -> name
    | Name (name, es) ->
       name ^ String.concat "" (List.map (fun e -> "[" ^ exp e ^ "]") es)
    | UnOp (op, e) -> "(" ^ unop op ^ exp e ^ ")"
    | BinOp (op, e1, e2) -> "(" ^ exp e1 ^ binop op ^ exp e2 ^ ")" in
  let rec format = function
    | Var (name, []) -> name
    | Var (name, i) ->
       name ^ String.concat "" (List.map (fun e -> "[" ^ exp e ^ "]") i)
    | Loop (name, s, t, f, _) ->
       "Loop(" ^ name ^ ", " ^ exp s ^ ", " ^ exp t ^ ") { " ^ format f ^ " }"
    | List (fs, _) -> List.map format fs |> String.concat " " in
  exp, format

exception FormatError of string

let loop =
  let lift f (range, e) = (range, f e) in
  let merge f (range1, e1) (range2, e2) =
    match range1, range2 with
    | None, None -> (None, f e1 e2)
    | Some r, None | None, Some r -> (Some r, f e1 e2)
    | Some r1, Some r2 ->
       if r1 = r2
       then (Some r1, f e1 e2)
       else raise (FormatError "range error") in
  let rec exp index e1 e2 =
    if e1 = e2
    then (None, e1)
    else ( match e1, e2 with
           | UnOp (op1, e1), UnOp (op2, e2) ->
              if op1 = op2
              then lift (fun x -> UnOp (op1, x)) @@ exp index e1 e2
              else (Some (e1, e2), Name (index, []))
           | BinOp (op1, e11, e12), BinOp (op2, e21, e22) ->
              if op1 = op2
              then merge (fun e1 e2 -> BinOp (op1, e1, e2))
                         (exp index e11 e21) (exp index e21 e22)
              else (Some (e1, e2), Name (index, []))
           | Name (s1, es1), Name (s2, es2) ->
              if s1 = s2
              then List.fold_right (merge List.cons) (List.map2 (exp index) es1 es2) (None, [])
                   |> lift (fun x -> Name (s1, x))
              else (Some (e1, e2), Name (index, []))
           | l, r -> (Some (l, r), Name (index, [])) ) in
  let rec format index x y =
    match x, y with
    | Var (s1, es1), Var (s2, es2) ->
       if s1 = s2
       then List.fold_right (merge List.cons) (List.map2 (exp index) es1 es2) (None, [])
            |> lift (fun x -> Var (s1, x))
       else raise (FormatError "range error")
    | Loop (s1, from1, to1, f1, delim1), Loop (s2, from2, to2, f2, delim2) ->
       if s1 = s2 && delim1 = delim2
       then let r1 = exp index from1 from2 in
            let r2 = exp index to1 to2 in
            let r3 = format index f1 f2 in
            let r4 = merge (fun x y -> (x, y)) r1 r2 in
            merge (fun (x, y) z -> Loop (s1, x, y, z, delim1)) r4 r3
       else raise (FormatError "range error")
    | List (fs1, delim1), List (fs2, delim2) ->
       if delim1 = delim2
       then List.fold_right (merge List.cons) (List.map2 (format index) fs1 fs2) (None, [])
            |> lift (fun x -> List (x, delim1))
       else raise (FormatError "format error")
    | _, _ -> raise (FormatError "format error") in
  let res x y index delim =
    let (range, f) = format index x y in
    match range with
    | Some (s, t) -> Loop (index, s, t, f, delim)
    | None -> raise (FormatError "format error") in res
