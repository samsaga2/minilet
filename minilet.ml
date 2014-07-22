let compile in_buffer =
  let lexbuf = Lexing.from_channel in_buffer in
  let ast = Parser.program Lexer.token lexbuf in
  Ast.pprint ast

let _ =
  compile stdin
