type sym = Symbol.t
type pos = Lexing.position
type typ = Types.t ref


type prog = decl list
 and decl = Decl of typ*sym*exp*pos
 and exp =
   | UnitExp of pos
   | IntExp of int*pos
   | ByteExp of int*pos
   | BoolExp of bool*pos
   | VarExp of sym*pos
   | CallExp of typ*exp*exp list*pos
   | LambdaExp of (typ*sym) list*exp*pos


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
  | VarExp (var,pos) ->
     Printf.sprintf "%s" (Symbol.name var)
  | CallExp (typ,fn,exps,pos) ->
     Printf.sprintf "(%s %s)"
		    (pprint_exp fn)
		    (pprint_exps exps)
  | LambdaExp (args,exp,pos) ->
     Printf.sprintf "[fun %s -> %s]"
		    (pprint_args args)
		    (pprint_exp exp)

and pprint_exps exps =
  String.concat " " (List.map pprint_exp exps)

and pprint_args args =
  let args = List.map (fun (typ,sym) ->
		       Printf.sprintf "%s:%s"
				      (Symbol.name sym)
				      (Types.pprint typ))
		      args in
  String.concat " " args


let pprint_decl = function
  | Decl (typ,var,exp,pos) ->
     Printf.sprintf "let %s:%s = %s\n"
		    (Symbol.name var)
		    (Types.pprint typ)
		    (pprint_exp exp)


let pprint prog =
  String.concat "\n" (List.map pprint_decl prog)
