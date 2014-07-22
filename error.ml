let error msg pos =
  let line = Lexer.line pos
  and col = Lexer.col pos in
  Printf.printf "%d:%d: %s\n%!" line col msg

let internal_error pos =
  error "internal error" pos

let not_implemented pos =
  error "not implemented" pos

let undeclared sym pos =
  error ("undeclared: "^(Symbol.name sym)) pos

let variable_expected pos =
  error "variable expected" pos

let function_expected pos =
  error "function expected" pos
