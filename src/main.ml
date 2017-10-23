exception FetchErrorxs

let uri = "http://arc083.contest.atcoder.jp/tasks/arc083_b"

let () =
  let (input_format, constraints) = Fetch.fetch uri in
  print_endline input_format;
  List.iter print_endline constraints;
  let format = Parser.toplevel Lexer.main (Lexing.from_string input_format) in
  print_endline (Syntax.pp format)
