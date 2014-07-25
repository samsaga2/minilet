open Ast

(* http://matt.might.net/articles/cps-conversion/ *)

let rec convert_exp_m exp =
  match exp with
   | LetExp _ ->
      Error.internal_error ()
   | UnitExp _
   | IntExp _
   | ByteExp _
   | BoolExp _
   | VarExp _ ->
      exp
   | CallExp (typ,sym,exps,pos) ->
      let ksym = Symbol.new_symbol () in
      let k = VarExp (Types.Undef, ksym,pos) in
      let exp = convert_exp_t exp k in
      LambdaExp ([ksym],exp,pos)
   | LambdaExp (syms,exp,pos) ->
      let ksym = Symbol.new_symbol () in
      let k = VarExp (Types.Undef,ksym,pos) in
      let syms = syms@[ksym] in
      let exp = convert_exp_t exp k in
      LambdaExp (syms,exp,pos)

and convert_exp_t exp k =
  match exp with
  | LetExp _ ->
      Error.internal_error ()
  | UnitExp (pos)
  | IntExp (_,pos)
  | ByteExp (_,pos)
  | BoolExp (_,pos)
  | VarExp (_,_,pos)
  | LambdaExp (_,_,pos) ->
     let exp = convert_exp_m exp in
     CallExp (Types.Undef,k,[exp],pos)
  | CallExp (typ,fnexp,bodyexps,pos) ->
     let rec make exps args =
       match exps with
       | [] ->
	  let args = args@[k] in
	  CallExp (Types.Undef,fnexp,args,pos)
       | hd::tl ->
	  let ksym = Symbol.new_symbol () in
	  let k = VarExp (Types.Undef,ksym,pos) in
	  let args = args@[k] in
	  let lambda = LambdaExp ([ksym],(make tl args),pos) in
	  convert_exp_t hd lambda
     in
     make bodyexps []


let convert_decl (sym,exp,pos) =
  let exp = convert_exp_m exp in
  (sym,exp,pos)


let rec convert_prog ast prog =
  match prog with
  | [] ->
     ast
  | (sym,exp,pos)::tl ->
     let exp = convert_exp_m exp in
     let ast = ast@[(sym,exp,pos)] in
     convert_prog ast tl


let convert prog =
  convert_prog [] prog
