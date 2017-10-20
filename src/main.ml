exception FetchErrorxs

let uri = "http://abc075.contest.atcoder.jp/tasks/abc075_a"

let () =
  let (input_format, constraints) = Fetch.fetch uri in
  print_endline input_format;
  List.iter print_endline constraints
