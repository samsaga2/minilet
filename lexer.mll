{
  open Lexing
  open Parser
  exception LexingError

  let update_loc lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {
      pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

  let line pos =
    pos.Lexing.pos_lnum

  let col pos =
    pos.Lexing.pos_cnum - pos.Lexing.pos_bol
}

let newline = ('\010' | '\013' | "\013\010")
let whitespace = [' ' '\t']
let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']

rule token = parse
  | eof                                 { EOF }
  | newline                             { update_loc lexbuf; token lexbuf }
  | whitespace+                         { token lexbuf }
  | "+"                                 { PLUS }
  | "-"                                 { MINUS }
  | "*"                                 { MUL }
  | "/"                                 { DIV }
  | "("                                 { LPAREN }
  | ")"                                 { RPAREN }
  | "=="				{ DEQ }
  | "="					{ EQ }
  | "true"				{ TRUE }
  | "false"				{ FALSE }
  | "let"                               { LET }
  | "in"                                { IN }
  | alpha (alpha|digit|"_")* as v	{ ID v }
  | digit+ as i "b"			{ BYTE (int_of_string i) }
  | digit+ as i				{ INT (int_of_string i) }
  | _					{ raise LexingError }
