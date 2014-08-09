open Astcore


let is_atom = function
  | UnitExp _ -> true
  | IntExp _ -> true
  | ByteExp _ -> true
  | BoolExp _ -> true
  | VarExp _ -> true
  | CallExp _ -> false
  | LambdaExp _ -> true


let is_atoms exps =
  List.for_all is_atom exps


let rec reduce_exp env exp =
  match exp with
   | UnitExp _
   | IntExp _
   | ByteExp _
   | BoolExp _ ->
      exp
   | VarExp (sym,pos) ->
      reduce_varexp env sym pos
   | LambdaExp (args,exp,pos) ->
      reduce_lambdaexp env args exp pos
   | CallExp (typ,exp,exps,pos) ->
      reduce_callexp env typ exp exps pos

and reduce_varexp env sym pos =
  match Env.get env sym with
  | None ->
     VarExp (sym,pos)
  | Some(exp) ->
     exp

and reduce_lambdaexp env args exp pos =
  let exp = reduce_exp env exp in
  LambdaExp (args,exp,pos)

and reduce_callexp env typ exp exps pos =
  let exp = reduce_exp env exp in
  let exps = List.map (reduce_exp env) exps in
  match exp with
  | LambdaExp (largs,lexp,lpos) when is_atoms exps ->
     let env = List.fold_left2
		 (fun env callexp (_,largsym) ->
		  Env.put env largsym callexp)
		 env exps largs in
     reduce_exp env lexp
  | _ ->
     CallExp (typ,exp,exps,pos)


let reduce_decl = function
  | Decl (typ,sym,exp,pos) ->
     let env = Env.empty in
     let betaexp = reduce_exp env exp in
     Decl (typ,sym,betaexp,pos)


let rec reduce_prog betaprog = function
  | [] ->
     betaprog
  | hd::tl ->
     let betadecl = reduce_decl hd in
     let betaprog = betaprog@[betadecl] in
     reduce_prog betaprog tl

let reduce prog =
  reduce_prog [] prog
