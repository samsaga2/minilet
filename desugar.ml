open Ast


let rec desugar_exp exp =
  match exp with
   | UnitExp _
   | IntExp _
   | ByteExp _
   | BoolExp _
   | VarExp _
   | CallExp _
   | LambdaExp _ ->
      exp
   | LetExp (sym,exp1,exp2,pos) ->
      (* let x=20 in x+1 ==> ((x -> x+1) 20) *)
      let exp1 = desugar_exp exp1
      and exp2 = desugar_exp exp2 in
      CallExp (Types.Undef,
	       LambdaExp (Types.Undef,[sym],exp2,pos),
	       [exp1], pos)


let desugar_decl = function
  | Decl (typ,sym,exp,pos) ->
     let exp = desugar_exp exp in
     Decl (typ,sym,exp,pos)


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
