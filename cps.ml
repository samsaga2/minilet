open Ast

(* http://matt.might.net/articles/cps-conversion/ *)

let rec convert_exp_m exp =
  match exp with
   | LetExp _ ->
      Error.internal_error ()
   | NilExp _
   | IntExp _
   | ByteExp _
   | BoolExp _
   | VarExp _ ->
      exp
   | CallExp (sym,exps,pos) ->
      let ksym = Symbol.new_symbol () in
      let k = VarExp (ksym,pos) in
      let exp = convert_exp_t exp k in
      LambdaExp ([ksym],exp,pos)
   | LambdaExp (syms,exp,pos) ->
      let ksym = Symbol.new_symbol () in
      let k = VarExp (ksym,pos) in
      let syms = syms@[ksym] in
      let exp = convert_exp_t exp k in
      LambdaExp (syms,exp,pos)

and convert_exp_t exp k =
  match exp with
  | LetExp _ ->
      Error.internal_error ()
  | NilExp (pos)
  | IntExp (_,pos)
  | ByteExp (_,pos)
  | BoolExp (_,pos)
  | VarExp (_,pos)
  | LambdaExp (_,_,pos) ->
     let exp = convert_exp_m exp in
     CallExp (k,[exp],pos)
  | CallExp (fnexp,bodyexps,pos) ->
     let rec make exps args =
       match exps with
       | [] ->
	  let args = args@[k] in
	  CallExp (fnexp,args,pos)
       | hd::tl ->
	  let ksym = Symbol.new_symbol () in
	  let k = VarExp (ksym,pos) in
	  let args = args@[k] in
	  let lambda = LambdaExp ([ksym],(make tl args),pos) in
	  convert_exp_t hd lambda
     in
     make bodyexps []


let convert_decl decl =
  match decl with
  | GlobalVar (sym,exp,pos) ->
     let exp = convert_exp_m exp in
     GlobalVar (sym,exp,pos)


let rec convert_prog ast prog =
  match prog with
  | [] ->
     ast
  | hd::tl ->
     let decl = convert_decl hd in
     convert_prog (ast@[decl]) tl


let convert prog =
  convert_prog [] prog
