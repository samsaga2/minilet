open Ast


module T = Types


let rec semant_exp exp =
  match exp with
   | UnitExp _
   | IntExp _
   | ByteExp _
   | BoolExp _
   | VarExp _
   | CallExp _
   | LambdaExp _
   | LetExp _ ->
      exp


let semant_decl = function
  | Decl (typ,sym,exp,pos) ->
     let exp = semant_exp exp in
     Decl (typ,sym,exp,pos)


let rec semant_prog desprog prog =
  match prog with
  | [] ->
     desprog
  | hd::tl ->
     let decl = semant_decl hd in
     let desprog = desprog@[decl] in
     semant_prog desprog tl


let semant prog =
  semant_prog [] prog
