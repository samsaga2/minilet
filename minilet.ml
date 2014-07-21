let compile in_buffer =
  let lexbuf = Lexing.from_channel in_buffer in
  let prog = Parser.program Lexer.token lexbuf in
  Ast.pprint prog

let _ =
  compile stdin
