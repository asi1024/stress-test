exception FetchErrorxs

let from_problem problem =
  let uri = "https://beta.atcoder.jp" ^ problem in
  try
    let (input_format, constraints, samples) = Fetch.get_problem_str uri in
    ( try
        let format = Parser.toplevel Lexer.main (Lexing.from_string input_format) in
        print_endline (Syntax.pp format);
        let typs = Typecheck.check_type_dim format [ List.hd samples ] in
        print_endline (Typecheck.pp_typs typs);
        print_endline "success"
      with
      | Typecheck.TypeCheckError mes -> print_endline mes
      | _ -> print_endline "parser error" )
  with _ -> print_endline "fetch error"

let from_contestid contest =
  print_endline contest;
  let task_uri = "https://beta.atcoder.jp/contests/" ^ contest ^ "/tasks" in
  let links = Fetch.get_problem_list task_uri in
  List.iter from_problem links

let () =
  (* from_problem "/contests/arc058/tasks/arc058_a" *)
  for i = 58 to 83 do
    from_contestid ("arc" ^ Printf.sprintf "%.3d" i)
  done;
  for i = 42 to 75 do
    from_contestid ("abc" ^ Printf.sprintf "%.3d" i)
  done
