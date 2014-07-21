type sym = Symbol.t
type typ = Symbol.t
type pos = Lexing.position

type prog = decl list
 and decl =
   | DeclareVar of sym*exp*pos
 and exp =
   | NilExp of pos
   | IntExp of int*pos
   | ByteExp of int*pos
   | BoolExp of bool*pos
   | VarExp of sym*pos
   | OpExp of op*exp*exp*pos
   | LetExp of sym*exp*exp*pos
 and op = AddOp | SubOp | MulOp | DivOp | EqOp

let rec pprint_exp = function
  | NilExp (pos) ->
     Printf.printf "nil"
  | IntExp (num,pos) ->
     Printf.printf "%d" num
  | ByteExp (num,pos) ->
     Printf.printf "%db" num
  | BoolExp (true,pos) ->
     Printf.printf "true"
  | BoolExp (false,pos) ->
     Printf.printf "false"
  | VarExp (var,pos) ->
     Printf.printf "%s" (Symbol.name var)
  | OpExp (op,left,right,pos) ->
     Printf.printf "(";
     pprint_exp left;
     Printf.printf (match op with
		    | AddOp -> "+"
		    | SubOp -> "-"
		    | MulOp -> "*"
		    | DivOp -> "/"
		    | EqOp -> "==");
     pprint_exp right;
     Printf.printf ")"
  | LetExp (var,exp,body,pos) ->
     Printf.printf "let %s=" (Symbol.name var);
     pprint_exp exp;
     Printf.printf " in ";
     pprint_exp body

let pprint_decl = function
  | DeclareVar (var,exp,pos) ->
     Printf.printf "let %s=" (Symbol.name var);
     pprint_exp exp;
     print_newline ()

let pprint prog =
  List.iter pprint_decl prog
