{
exception LexerError of string

}

rule tag = parse
  "/"? ['a'-'z']+ ">"
    { let tag_words = function
        | "var" | "/var" -> main lexbuf
        | "pre" -> Parser.BPRE
        | "/pre" -> Parser.EPRE
        | _ -> raise Not_found in
      let s = Lexing.lexeme lexbuf in
      let id = String.sub s 0 (String.length s - 1) in
      try tag_words id
      with _ -> raise (LexerError "Unknown tag") }

and main = parse
  [' ' '\009' '\012']+  { Parser.SPACE }
| ' '* '\n'+ ' '*       { Parser.EOLN }
| "<" { tag lexbuf }

| ","    { Parser.COMMA }
| "_"    { Parser.UNDER }
| "{"    { Parser.LBRACE }
| "}"    { Parser.RBRACE }
| ":"    { Parser.DOTS }
| "…"   { Parser.DOTS }
| "." ['.']+ { Parser.DOTS }

| "("    { Parser.LPAREN }
| ")"    { Parser.RPAREN }
| "+"    { Parser.PLUS }
| "-"    { Parser.MINUS }
| "/"    { Parser.DIV }
(* | "&lt;" { Parser.LT } *)
(* | "&gt;" { Parser.GT } *)

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| ['A'-'Z']  { Parser.ID (Lexing.lexeme lexbuf) }
| ['a'-'z']+ { Parser.ID (Lexing.lexeme lexbuf) }

| eof { Parser.EOF }
