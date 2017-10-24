open Syntax

exception TypeCheckError of string

let insert (name, value) assoc =
  try
    if List.assoc name assoc = value
    then assoc
    else raise (TypeCheckError "type error")
  with Not_found -> (name, value) :: assoc

let append assoc1 assoc2 = List.fold_right insert assoc1 assoc2

let concat assoc_list = List.fold_left append [] assoc_list

let check_dim fmt =
  let rec exp = function
    | Const _ -> []
    | Name (name, es) -> (name, List.length es) :: concat (List.map exp es)
    | UnOp (_, e) -> exp e
    | BinOp (_, e1, e2) -> append (exp e1) (exp e2) in
  let rec format = function
    | Var (name, es) -> (name, List.length es) :: concat (List.map exp es)
    | Loop (index, e1, e2, fmt, _) ->
       let assoc = concat [exp e1; exp e2; format fmt] in
       if List.mem_assoc index assoc
       then List.remove_assoc index assoc
       else assoc
    | List (fmts, _) -> concat (List.map format fmts) in
  format fmt

type ty = Int | Real | String
type tyval = IntV of int | RealV of float | StringV of string

let string_of_ty = function
  | Int -> "int"
  | Real -> "real"
  | String -> "string"

let string_of_tyval = function
  | IntV x -> string_of_int x
  | RealV x -> string_of_float x
  | StringV x -> x

let insert_ty (name, value) assoc =
  try
    let old = List.assoc name assoc in
    (name, max old value) :: (List.remove_assoc name assoc)
  with Not_found -> (name, value) :: assoc

let append_ty assoc1 assoc2 = List.fold_right insert_ty assoc1 assoc2

let concat_ty assoc_list = List.fold_left append_ty [] assoc_list

let regexp_int = Str.regexp "[1-9][0-9]*"
let regexp_real = Str.regexp "[0-9]*\\.[0-9]"

let tyval_of_string s =
  if Str.string_match regexp_int s 0 then (Int, IntV (int_of_string s))
  else if Str.string_match regexp_real s 0 then (Real, RealV (float_of_string s))
  else (String, StringV s)

let string_of_variable s index =
  s ^ "{" ^ String.concat "," (List.map string_of_int index) ^ "}"

let rec take_after n l =
  if n <= 0 then l
  else match l with
       | [] -> raise (TypeCheckError "error")
       | _ :: t -> take_after (n - 1) t

let rec eval assoc e =
  match e with
  | Const i -> i
  | Name (name, es) ->
     let es = List.map (eval assoc) es in
     let s = string_of_variable name es in
     ( match List.assoc s assoc with
       | IntV i -> i
       | _ -> raise (TypeCheckError "type error") )
  | UnOp (Neg, e) -> -(eval assoc e)
  | BinOp (Plus, e1, e2) -> eval assoc e1 + eval assoc e2
  | BinOp (Minus, e1, e2) -> eval assoc e1 - eval assoc e2
  | BinOp (Mult, e1, e2) -> eval assoc e1 * eval assoc e2
  | BinOp (Div, e1, e2) -> eval assoc e1 / eval assoc e2

let check_type fmt inputs =
  let assoc = ref [] in
  let assoc_ty = ref [] in
  let rec line fmts l =
    match fmts with
    | [] -> if l = [] then [] else raise (TypeCheckError "error")
    | Var (name, es) :: tail ->
       ( match l with
         | [] -> raise (TypeCheckError "error")
         | h :: t ->
            (* print_endline h; *)
            let index = List.map (eval !assoc) es in
            let var = string_of_variable name index in
            let (ty, tyval) = tyval_of_string h in
            assoc := insert (var, tyval) !assoc;
            assoc_ty := insert_ty (name, ty) !assoc_ty;
            line tail t )
    | Loop (index, s, t, f, _) :: tail ->
       let s = eval !assoc s in
       let t = eval !assoc t in
       ( if List.length l < t - s then raise (TypeCheckError "error") );
       for i = s to t do
         let index = string_of_variable index [] in
         assoc := insert (index, IntV i) !assoc;
         line [ f ] [ List.nth l (i - s) ] |> ignore;
         assoc := List.remove_assoc index !assoc
       done;
       line tail (take_after (t - s + 1) l)
    | _ -> raise (TypeCheckError "internal error") in
  let rec lines fmts l = 
    match fmts with
    | [] -> []
    | Var (_, _) :: _ -> raise (TypeCheckError "internal error")
    | Loop (index, s, t, f, _) :: tail ->
       let s = eval !assoc s in
       let t = eval !assoc t in
       ( if List.length l < t - s then raise (TypeCheckError "error") );
       for i = s to t do
         let index = string_of_variable index [] in
         assoc := insert (index, IntV i) !assoc;
         lines [ f ] [ List.nth l (i - s) ] |> ignore;
         assoc := List.remove_assoc index !assoc
       done;
       lines tail (take_after (t - s + 1) l)
    | List (fs, _) :: tail ->
       match l with
       | [] -> raise (TypeCheckError "error")
       | h :: t ->
          line fs (String.split_on_char ' ' h) |> ignore;
          lines tail t in
  let testcase input =
    assoc := [];
    match fmt with
    | List (fmts, _) -> lines fmts (String.split_on_char '\n' input) |> ignore
    | _ -> raise (TypeCheckError "error") in
  List.iter testcase inputs;
  !assoc_ty

type typ = { dim : int; ty : ty; }

let string_of_typ x =
  "{ dim = " ^ string_of_int x.dim ^ "; " ^ string_of_ty x.ty ^ " }"

let check_type_dim fmt samples =
  let dim = check_dim fmt in
  let types = check_type fmt samples in
  List.map (fun (var, d) -> (var, { dim = d; ty = List.assoc var types })) dim
                          
let pp_typs typs =
  let pp (var, typ) = var ^ " : " ^ string_of_typ typ in
  List.map pp typs |> String.concat "\n"
