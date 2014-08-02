open Ast


module T = Types
module E = Error


let rec assert_type typ1 typ2 pos =
  match (!typ1,!typ2) with
  | (T.Error,_) -> ()
  | (_,T.Error) -> ()
  | (T.Undef,_) -> typ1:=!typ2
  | (_,T.Undef) -> typ2:=!typ1
  | (T.Fun(t1a,t1b),T.Fun(t2a,t2b)) ->
    assert_type t1a t2a pos;
    assert_type t1b t2b pos
  | _ ->
    E.type_error pos


let assert_arg_types args lambda pos =
    match args with
    | [] -> lambda
    | hdarg::args ->
      match lambda with
      | hd::[] ->
         E.missing_arguments pos;
	 ref T.Error
      | hdlambda::lambda ->
         assert_type hdarg hdlambda pos;
         assert_arg_types args lambda

let rec semant_exp env exp =
  match exp with
   | UnitExp _ -> ref T.Unit
   | IntExp _ -> ref T.Int
   | ByteExp _ -> ref T.Byte
   | BoolExp _ -> ref T.Bool
   | VarExp (typ,sym,pos) ->
      semant_var env typ sym pos
   | CallExp (typ,exp,args,pos) ->
      semant_call env typ exp args pos
   | LambdaExp (args,exp,pos) ->
      semant_lambda env args exp pos

and semant_call env typ exp args pos =
  let typargs = List.map (semant_exp env) args in
  let lambdatyp = semant_exp env exp in

  match !lambdatyp with
  | T.Fun _ ->
     assert_arg_types typargs lambdatyp pos
  | _ ->
     E.function_expected pos;
     ref T.Error

and semant_lambda env args exp pos =
  let env = List.fold_left
	      (fun env (typarg,symarg) -> Env.put env symarg typarg)
	      env args in
  let typexp = semant_exp env exp in
  let typargs = List.map (fun (typ,_) -> typ) args in
  ref (T.Fun (typargs@[typexp]))

and semant_var env typ sym pos =
  match Env.get env sym with
  | Some(envtyp) ->
     assert_type typ envtyp pos;
     typ
  | None ->
     E.undeclared sym pos;
     ref T.Error


let semant_decl env = function
  | Decl (typ,sym,exp,pos) ->
     let semtyp = semant_exp env exp in
     assert_type typ semtyp pos


let rec semant_prog env prog =
  match prog with
  | [] ->
     ()
  | hd::tl ->
     semant_decl env hd;
     semant_prog env tl


let semant prog =
  let env = Env.empty in
  let env = Env.put env
		    (Symbol.symbol "+")
		    (ref (T.Fun [ref T.Int; ref T.Int; ref T.Int])) in
  semant_prog env prog
