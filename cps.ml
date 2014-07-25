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
      LambdaExp (typ,[(Types.Undef,ksym)],exp,pos)
   | LambdaExp (typ,syms,exp,pos) ->
      let ksym = Symbol.new_symbol () in
      let k = VarExp (Types.Undef,ksym,pos) in
      let syms = syms@[(Types.Undef,ksym)] in
      let exp = convert_exp_t exp k in
      LambdaExp (typ,syms,exp,pos)

and convert_exp_t exp k =
  match exp with
  | LetExp _ ->
      Error.internal_error ()
  | UnitExp (pos)
  | IntExp (_,pos)
  | ByteExp (_,pos)
  | BoolExp (_,pos)
  | VarExp (_,_,pos)
  | LambdaExp (_,_,_,pos) ->
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
	  let lambda = LambdaExp (Types.Undef,[(Types.Undef,ksym)],(make tl args),pos) in
	  convert_exp_t hd lambda
     in
     make bodyexps []


let convert_decl = function
  | Decl (typ,sym,exp,pos) ->
     let cpsexp = convert_exp_m exp in
     Decl (typ,sym,cpsexp,pos)


let rec convert_prog cpsprog prog =
  match prog with
  | [] ->
     cpsprog
  | hd::tl ->
     let cpsdecl = convert_decl hd in
     let cpsprog = cpsprog@[cpsdecl] in
     convert_prog cpsprog tl


let convert prog =
  convert_prog [] prog
