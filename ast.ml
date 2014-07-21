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
   | CallExp of sym*exp list*pos
   | LetExp of sym*exp*exp*pos

let rec pprint_exp = function
  | NilExp (pos) ->
     Printf.sprintf "nil"
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
  | CallExp (name,args,pos) ->
     let args = String.concat ","
			      (List.map (fun arg ->
					 "("^pprint_exp arg^")") args) in
     Printf.sprintf "%s(%s)" (Symbol.name name) args
  | LetExp (var,exp,body,pos) ->
     Printf.sprintf "let %s=%s in %s"
		    (Symbol.name var)
		    (pprint_exp exp)
		    (pprint_exp body)

let pprint_decl = function
  | DeclareVar (var,exp,pos) ->
     Printf.printf "let %s=%s\n"
		   (Symbol.name var)
		   (pprint_exp exp)

let pprint prog =
  List.iter pprint_decl prog
