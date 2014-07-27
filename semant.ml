open Ast


module T = Types


let assert_type typ1 typ2 pos =
  if !typ1=T.Error || !typ2=T.Error then
    Error.type_error pos
  else if !typ1=T.Undef then
    typ1:=!typ2
  else if !typ2=T.Undef then
    typ2:=!typ1
  else if !typ2<>T.Undef && !typ1<> !typ2 then
    Error.type_error pos


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
   | LetExp _ ->
      Error.internal_error ()

and semant_call env typ exp args pos =
  let typargs = List.map (semant_exp env) args in
  let exptyp = semant_exp env exp in
  match !exptyp with
  | T.Fun(typlambda) ->
     let rec make typargs typlambda =
       match typargs with
       | [] -> typlambda
       | typhdarg::typtlargs ->
	  let typhdlambda = List.hd typlambda
	  and typtllambda = List.tl typlambda in
	  assert_type typhdarg typhdlambda pos;
	  make typtlargs typtllambda
     in
     let typreturn = make (typargs@[exptyp]) typlambda in
     let typreturn = match typreturn with
       | [] -> ref T.Unit
       | hd::[] -> hd
       | _ -> ref (T.Fun typreturn) in
     assert_type typ typreturn pos;
     exptyp
  | _ ->
     Error.function_expected pos;
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
     Error.undeclared sym pos;
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
