open Astcore


let rec equivalence_exp env exp =
  match exp with
   | UnitExp _
   | IntExp _
   | ByteExp _
   | BoolExp _ ->
      exp
   | VarExp (sym,pos) ->
      equivalence_varexp env sym pos
   | LambdaExp (args,exp,pos) ->
      equivalence_lambdaexp env args exp pos
   | CallExp (typ,exp,exps,pos) ->
      equivalence_callexp env typ exp exps pos

and equivalence_varexp env sym pos =
  match Env.get env sym with
  | None ->
     VarExp (sym,pos)
  | Some(sym) ->
     VarExp (sym,pos)

and equivalence_lambdaexp env args exp pos =
  let (env,args) = List.fold_left
		     (fun (env,args) (typarg,symarg) ->
		      let newsymarg = Symbol.new_symbol () in
		      let env = Env.put env symarg newsymarg in
		      let arg = (typarg,newsymarg) in
		      let args = args@[arg] in
		      (env,args))
		     (env,[]) args in
  let exp = equivalence_exp env exp in
  LambdaExp (args,exp,pos)

and equivalence_callexp env typ exp exps pos =
  let exp = equivalence_exp env exp in
  let exps = List.map (equivalence_exp env) exps in
  CallExp (typ,exp,exps,pos)


let equivalence_decl = function
  | Decl (typ,sym,exp,pos) ->
     let env = Env.empty in
     let alphaexp = equivalence_exp env exp in
     Decl (typ,sym,alphaexp,pos)


let rec equivalence_prog alphaprog = function
  | [] ->
     alphaprog
  | hd::tl ->
     let alphadecl = equivalence_decl hd in
     let alphaprog = alphaprog@[alphadecl] in
     equivalence_prog alphaprog tl

let equivalence prog =
  equivalence_prog [] prog
