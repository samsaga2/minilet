%{
  open Ast

  module S = Symbol
  module T = Types

  let funiii = T.Fun (T.Int, T.Fun(T.Int, T.Int))
%}

%token <int> INT
%token <int> BYTE
%token <string> ID
%token EOF PLUS MINUS MUL DIV
%token LPAREN RPAREN
%token EQ DEQ
%token TRUE FALSE
%token LET IN
%token FUN ARROW

%nonassoc ARROW
%nonassoc IN
%left PLUS MINUS
%left MUL DIV
%nonassoc DEQ
%nonassoc UMINUS

%start <Ast.prog> program

%%

program:
  | l=list(decl) EOF
    { l }

decl:
  | LET v=id EQ e=exp2
    { (v,e,$startpos) }
  | LET v=id args=nonempty_list(id) EQ e=exp2
    { (v,LambdaExp (args,e,$startpos),$startpos) }
  | LET v=id LPAREN RPAREN EQ e=exp2
    { (v,LambdaExp ([],e,$startpos),$startpos) }

exp:
  | INT
    { IntExp ($1,$startpos) }
  | BYTE
    { ByteExp ($1,$startpos) }
  | TRUE
    { BoolExp (true,$startpos) }
  | FALSE
    { BoolExp (false,$startpos) }
  | id
    { VarExp (T.Undef, $1,$startpos) }
  | LPAREN e=exp2 RPAREN
    { e }

exp2:
  | MINUS e=exp2 %prec UMINUS
    { CallExp ((VarExp (funiii, S.symbol "-",$startpos)),
	       [IntExp (0,$startpos);e], $startpos) }
  | left=exp2 PLUS right=exp2
    { CallExp ((VarExp (funiii, S.symbol "+",$startpos)),
	       [left;right], $startpos) }
  | left=exp2 MINUS right=exp2
    { CallExp ((VarExp (funiii, S.symbol "-",$startpos)),
	       [left;right],$startpos) }
  | left=exp2 MUL right=exp2
    { CallExp ((VarExp (funiii, S.symbol "*",$startpos)),
	       [left;right],$startpos) }
  | left=exp2 DIV right=exp2
    { CallExp ((VarExp (funiii, S.symbol "/",$startpos)),
	       [left;right],$startpos) }
  | left=exp2 DEQ right=exp2
    { CallExp ((VarExp (funiii, S.symbol "==",$startpos)),
	       [left;right],$startpos) }
  | LET v=id EQ e=exp2 IN b=exp2
    { LetExp (v,e,b,$startpos) }
  | FUN args=list(id) ARROW e=exp2
    { LambdaExp (args,e,$startpos) }
  | LPAREN RPAREN
    { UnitExp ($startpos) }
  | exp LPAREN RPAREN
    { CallExp ($1,[],$startpos) }
  | exp nonempty_list(exp)
    { CallExp ($1,$2,$startpos) }
  | exp
    { $1 }

id:
  | ID
    { S.symbol $1 }
