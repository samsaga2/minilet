open Ast


module T = Types
module A = Astcore

let rec desugar_exp exp =
  match exp with
   | UnitExp (pos) -> A.UnitExp (pos)
   | IntExp (num,pos) -> A.IntExp (num,pos)
   | ByteExp (num,pos) -> A.ByteExp (num,pos)
   | BoolExp (num,pos) -> A.BoolExp (num,pos)
   | VarExp (typ,sym,pos) -> A.VarExp (typ,sym,pos)
   | CallExp (typ,exp,exps,pos) ->
      let exp = desugar_exp exp
      and exps = desugar_exps exps in
      A.CallExp (typ,exp,exps,pos)
   | LambdaExp (args,exp,pos) ->
      let exp = desugar_exp exp in
      A.LambdaExp (args,exp,pos)
   | LetExp (sym,exp1,exp2,pos) ->
      (* let x=20 in x+1 ==> ((x -> x+1) 20) *)
      let exp1 = desugar_exp exp1
      and exp2 = desugar_exp exp2 in
      A.CallExp (ref T.Undef,
		 A.LambdaExp ([(ref T.Undef,sym)],exp2,pos),
		 [exp1], pos)

and desugar_exps exps =
  List.map desugar_exp exps

let desugar_decl = function
  | Decl (typ,sym,exp,pos) ->
     let exp = desugar_exp exp in
     A.Decl (typ,sym,exp,pos)


let rec desugar_prog desprog prog =
  match prog with
  | [] ->
     desprog
  | hd::tl ->
     let decl = desugar_decl hd in
     let desprog = desprog@[decl] in
     desugar_prog desprog tl


let desugar prog =
  desugar_prog [] prog
