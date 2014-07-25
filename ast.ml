type sym = Symbol.t
type pos = Lexing.position
type typ = Types.t


type prog = decl list
 and decl = Decl of typ*sym*exp*pos
 and exp =
   | UnitExp of pos
   | IntExp of int*pos
   | ByteExp of int*pos
   | BoolExp of bool*pos
   | VarExp of typ*sym*pos
   | CallExp of typ*exp*exp list*pos
   | LetExp of sym*exp*exp*pos
   | LambdaExp of sym list*exp*pos


let rec pprint_exp = function
  | UnitExp (pos) ->
     Printf.sprintf "()"
  | IntExp (num,pos) ->
     Printf.sprintf "%d" num
  | ByteExp (num,pos) ->
     Printf.sprintf "%db" num
  | BoolExp (true,pos) ->
     Printf.sprintf "true"
  | BoolExp (false,pos) ->
     Printf.sprintf "false"
  | VarExp (typ,var,pos) ->
     Printf.sprintf "%s:%s" (Symbol.name var) (Types.pprint typ)
  | CallExp (typ,fn,exps,pos) ->
     Printf.sprintf "(%s %s):%s"
		    (pprint_exp fn)
		    (pprint_exps exps)
		    (Types.pprint typ)
  | LetExp (var,exp,body,pos) ->
     Printf.sprintf "let %s = %s in %s"
		    (Symbol.name var)
		    (pprint_exp exp)
		    (pprint_exp body)
  | LambdaExp (args,exp,pos) ->
     Printf.sprintf "[fun %s -> %s]"
		    (pprint_syms args)
		    (pprint_exp exp)

and pprint_exps exps =
  String.concat " " (List.map pprint_exp exps)

and pprint_syms syms =
  String.concat " " (List.map Symbol.name syms)


let pprint_decl = function
  | Decl (typ,var,exp,pos) ->
     Printf.sprintf "let %s:%s = %s\n"
		    (Symbol.name var)
		    (Types.pprint typ)
		    (pprint_exp exp)


let pprint prog =
  String.concat "\n" (List.map pprint_decl prog)
