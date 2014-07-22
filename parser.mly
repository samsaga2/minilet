%{
  open Ast
  module S = Symbol
%}

%token <int> INT
%token <int> BYTE
%token <string> ID
%token EOF PLUS MINUS MUL DIV
%token LPAREN RPAREN SEMICOLON
%token EQ DEQ
%token TRUE FALSE
%token LET IN
%token FUN ARROW

%nonassoc ARROW
%left IN SEMICOLON
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
  | LET v=id EQ e=exp
    { GlobalVar (v,e,$startpos) }
  | LET v=id LPAREN RPAREN EQ e=exp
    { GlobalFun (v,[],e,$startpos) }

exp:
  | e1=exp SEMICOLON e2=exp
    { LetExp (S.symbol "_", e1, e2, $startpos) }
  | INT
    { IntExp ($1,$startpos) }
  | BYTE
    { ByteExp ($1,$startpos) }
  | TRUE
    { BoolExp (true,$startpos) }
  | FALSE
    { BoolExp (false,$startpos) }
  | id
    { VarExp ($1,$startpos) }
  | MINUS e=exp %prec UMINUS
    { CallExp (VarExp (S.symbol "-", $startpos), [IntExp (0,$startpos);e],$startpos) }
  | left=exp PLUS right=exp
    { CallExp (VarExp (S.symbol "+", $startpos), [left;right],$startpos) }
  | left=exp MINUS right=exp
    { CallExp (VarExp (S.symbol "-", $startpos), [left;right],$startpos) }
  | left=exp MUL right=exp
    { CallExp (VarExp (S.symbol "*", $startpos), [left;right],$startpos) }
  | left=exp DIV right=exp
    { CallExp (VarExp (S.symbol "/", $startpos), [left;right],$startpos) }
  | left=exp DEQ right=exp
    { CallExp (VarExp (S.symbol "==", $startpos),[left;right],$startpos) }
  | LPAREN RPAREN
    { NilExp ($startpos) }
  | LPAREN e=exp RPAREN
    { e }
  | LET v=id EQ e=exp IN b=exp
    { LetExp (v,e,b,$startpos) }
  | FUN args=list(id) ARROW e=exp
    { LambdaExp (args,e,$startpos) }

id:
  | ID
    { S.symbol $1 }
