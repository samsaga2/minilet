open Ast


module T = Types


let assert_type typ1 typ2 pos =
  if !typ1=T.Undef then
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
   | CallExp _ ->
      (* TODO:  *)
      ref T.Unit
   | LambdaExp _ ->
      (* TODO:  *)
      ref T.Unit
   | LetExp _ ->
      (* TODO:  *)
      ref T.Unit

and semant_var env typ sym pos =
  match Env.get env sym with
  | Some(envtyp) ->
     assert_type typ envtyp pos;
     typ
  | None ->
     Error.undeclared sym pos;
     ref T.Unit


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
  semant_prog env prog
