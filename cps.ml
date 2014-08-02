open Astcore


module T = Types
module S = Symbol
module E = Error


(* http://matt.might.net/articles/cps-conversion/ *)

let rec convert_exp_m exp =
  match exp with
   | UnitExp _
   | IntExp _
   | ByteExp _
   | BoolExp _
   | VarExp _ ->
      exp
   | CallExp (typ,sym,exps,pos) ->
      let ksym = S.new_symbol () in
      let k = VarExp (ref T.Undef, ksym,pos) in
      let exp = convert_exp_t exp k in
      LambdaExp ([(ref T.Undef,ksym)],exp,pos)
   | LambdaExp (syms,exp,pos) ->
      let ksym = S.new_symbol () in
      let k = VarExp (ref T.Undef,ksym,pos) in
      let syms = syms@[(ref T.Undef,ksym)] in
      let exp = convert_exp_t exp k in
      LambdaExp (syms,exp,pos)

and convert_exp_t exp k =
  match exp with
  | UnitExp (pos)
  | IntExp (_,pos)
  | ByteExp (_,pos)
  | BoolExp (_,pos)
  | VarExp (_,_,pos)
  | LambdaExp (_,_,pos) ->
     let exp = convert_exp_m exp in
     CallExp (ref T.Undef,k,[exp],pos)
  | CallExp (typ,fnexp,bodyexps,pos) ->
     let rec make exps args =
       match exps with
       | [] ->
	  let args = args@[k] in
	  CallExp (ref T.Undef,fnexp,args,pos)
       | hd::tl ->
	  let ksym = S.new_symbol () in
	  let k = VarExp (ref T.Undef,ksym,pos) in
	  let args = args@[k] in
	  let lambda = LambdaExp ([(ref T.Undef,ksym)],(make tl args),pos) in
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
