exception FetchError

let fetch uri =
  try let connection = Curl.init () in
      let write_buff = Buffer.create 1999 in
      let writef x = Buffer.add_string write_buff x; String.length x in
      Curl.set_writefunction connection writef;
      Curl.set_url connection uri;
      Curl.perform connection;
      Curl.global_cleanup ();
      Buffer.contents write_buff;
  with _ -> raise FetchError

open Soup

let get_problem_list uri =
  parse (fetch uri) $$ "td" |> to_list
  |> List.map (fun s -> s $$ "a" |> to_list)
  |> List.concat |> List.map to_string
  |> List.map (fun s -> List.nth (String.split_on_char '\"' s) 1)
  |> List.filter (fun s -> Str.string_match (Str.regexp ".*tasks.*") s 0)
  |> List.sort_uniq compare

let get_input_format nodes =
  let is node =
    match node $? "h3" with
    | Some x -> leaf_text x = Some "Input"
                || leaf_text x = Some "Inputs"
    | None -> false in
  match List.filter is nodes with
  | [x] -> x $ "pre" |> to_string
  | _ -> raise Not_found

let get_constraints nodes =
  let is node =
    match node $? "h3" with
    | Some x -> leaf_text x = Some "Constraints"
    | None -> false in
  match List.filter is nodes with
  | [x] -> x $$ "li" |> to_list |> List.map to_string
  | _ -> raise Not_found

let get_problem_str uri =
  let soup = parse (fetch uri) $$ "section" |> to_list in
  let input_format = get_input_format soup in
  let constraints = get_constraints soup in
  (input_format, constraints)
