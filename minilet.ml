let compile in_buffer =
  let lexbuf = Lexing.from_channel in_buffer in
  let ast = Parser.program Lexer.token lexbuf in
  let ast = Desugar.desugar ast in
  let ast = Cps.convert ast in
  let ast = Alpha.equivalence ast in
  let ast = Beta.reduce ast in
  Semant.semant ast;
  print_endline (Astcore.pprint ast)

let _ =
  compile stdin
