open Ast


let rec desugar_exp exp =
  match exp with
   | NilExp _
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
      CallExp (LambdaExp ([sym],exp2,pos), [exp1], pos)


let desugar_decl decl =
  match decl with
  | GlobalVar (sym,exp,pos) ->
     let exp = desugar_exp exp in
     GlobalVar (sym,exp,pos)


let rec desugar_prog ast prog =
  match prog with
  | [] ->
     ast
  | hd::tl ->
     let decl = desugar_decl hd in
     desugar_prog (ast@[decl]) tl


let desugar prog =
  desugar_prog [] prog