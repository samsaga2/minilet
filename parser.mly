%{
  open Ast

  module S = Symbol
  module T = Types
%}

%token <int> INT
%token <int> BYTE
%token <string> ID
%token EOF PLUS MINUS MUL DIV
%token LPAREN RPAREN
%token EQ DEQ
%token TRUE FALSE
%token LET IN
%token FUN ARROW COLON DOT
%token TYPINT TYPBYTE TYPBOOL TYPUNIT

%right DOT
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
    { Decl (Types.Undef,
	    v, e,
	    $startpos) }
  | LET v=id COLON t=typ EQ e=exp2
    { Decl (t, v, e,
	    $startpos) }
  | LET v=id args=args EQ e=exp2
    { Decl (Types.Undef,
	    v, LambdaExp (Types.Undef,args,e,$startpos),
	    $startpos) }
  | LET v=id args=args COLON t=typ EQ e=exp2
    { Decl (Types.Undef, v, LambdaExp (t,args,e,$startpos),
	    $startpos) }

args:
  | nonempty_list(id)
    { $1 }
  | LPAREN RPAREN
    { [] }

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
    { CallExp (Types.Undef,
	       (VarExp (Types.Undef, S.symbol "-",$startpos)),
	       [IntExp (0,$startpos);e], $startpos) }
  | left=exp2 PLUS right=exp2
    { CallExp (Types.Undef,
	       (VarExp (Types.Undef, S.symbol "+",$startpos)),
	       [left;right], $startpos) }
  | left=exp2 MINUS right=exp2
    { CallExp (Types.Undef,
	       (VarExp (Types.Undef, S.symbol "-",$startpos)),
	       [left;right],$startpos) }
  | left=exp2 MUL right=exp2
    { CallExp (Types.Undef,
	       (VarExp (Types.Undef, S.symbol "*",$startpos)),
	       [left;right],$startpos) }
  | left=exp2 DIV right=exp2
    { CallExp (Types.Undef,
	       (VarExp (Types.Undef, S.symbol "/",$startpos)),
	       [left;right],$startpos) }
  | left=exp2 DEQ right=exp2
    { CallExp (Types.Undef,
	       (VarExp (Types.Undef, S.symbol "==",$startpos)),
	       [left;right],$startpos) }
  | LET v=id EQ e=exp2 IN b=exp2
    { LetExp (v,e,b,$startpos) }
  | FUN args=list(id) ARROW e=exp2
    { LambdaExp (Types.Undef,args,e,$startpos) }
  | LPAREN RPAREN
    { UnitExp ($startpos) }
  | exp LPAREN RPAREN
    { CallExp (Types.Undef,$1,[],$startpos) }
  | exp nonempty_list(exp)
    { CallExp (Types.Undef,$1,$2,$startpos) }
  | exp
    { $1 }

id:
  | ID
      { S.symbol $1 }

typ:
  | TYPINT
     { T.Int }
  | TYPBYTE
     { T.Byte }
  | TYPUNIT
     { T.Unit }
  | TYPBOOL
     { T.Bool }
  | typ DOT typ
     { T.Fun ($1,$3) }
