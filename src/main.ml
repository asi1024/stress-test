exception FetchErrorxs

let uri = "http://abc075.contest.atcoder.jp/tasks/abc075_a"

let () =
  let (input_format, constraints) = Fetch.fetch uri in
  print_endline input_format;
  List.iter print_endline constraints;
  let input_format = "<pre><var>a_1</var> <var>a_n</var></pre>" in
  let format = Parser.toplevel Lexer.main (Lexing.from_string input_format) in
  print_endline (Syntax.pp format)
