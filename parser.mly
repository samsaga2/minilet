%{
  open Ast
%}

%token <int> INT
%token <int> BYTE
%token <string> ID
%token EOF PLUS MINUS MUL DIV
%token LPAREN RPAREN SEMICOLON
%token EQ DEQ
%token TRUE FALSE
%token LET IN

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
  | var_decl { $1 }

var_decl:
  | LET v=id EQ e=exp
    { DeclareVar (v,e,$startpos) }

exp:
  | e1=exp SEMICOLON e2=exp
    { LetExp (Symbol.symbol "_", e1, e2, $startpos) }
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
    { OpExp (SubOp, IntExp (0,$startpos),e,$startpos) }
  | left=exp PLUS right=exp
    { OpExp (AddOp,left,right,$startpos) }
  | left=exp MINUS right=exp
    { OpExp (SubOp,left,right,$startpos) }
  | left=exp MUL right=exp
    { OpExp (MulOp,left,right,$startpos) }
  | left=exp DIV right=exp
    { OpExp (DivOp,left,right,$startpos) }
  | left=exp DEQ right=exp
    { OpExp (EqOp,left,right,$startpos) }
  | LPAREN RPAREN
    { NilExp ($startpos) }
  | LPAREN e=exp RPAREN
    { e }
  | LET v=id EQ e=exp IN b=exp
    { LetExp (v,e,b,$startpos) }

id:
  | ID
    { Symbol.symbol $1 }
