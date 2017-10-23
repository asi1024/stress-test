{
exception LexerError of string

let tag_words = [
  ("var",  Parser.BVAR);
  ("/var", Parser.EVAR);
  ("pre",  Parser.BPRE);
  ("/pre", Parser.EPRE);
] 
}

rule tag = parse
  "/"? ['a'-'z']+ ">"
    { let s = Lexing.lexeme lexbuf in
      let id = String.sub s 0 (String.length s - 1) in
      try List.assoc id tag_words
      with _ -> raise (LexerError "Unknown tag") }

and main = parse
  [' ' '\009' '\012']+  { Parser.SPACE }
| ['\n']+           { Parser.EOLN }
| "<" { tag lexbuf }

| ","    { Parser.COMMA }
| "_"    { Parser.UNDER }
| "{"    { Parser.LBRACE }
| "}"    { Parser.RBRACE }
| "." ['.']+ { Parser.HDOTS }
| ":"        { Parser.VDOTS }

| "("    { Parser.LPAREN }
| ")"    { Parser.RPAREN }
| "+"    { Parser.PLUS }
| "-"    { Parser.MINUS }
| "/"    { Parser.DIV }
(* | "&lt;" { Parser.LT } *)
(* | "&gt;" { Parser.GT } *)

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| ['a'-'z' 'A'-'Z']
    { Parser.ID (Lexing.lexeme lexbuf) }

| eof { Parser.EOF }
