type sym = Symbol.t

type t =
  | Error
  | Undef
  | Unit
  | Byte
  | Int
  | Bool
  | Fun of t ref list

let rec pprint typ =
  match !typ with
  | Error       -> "error"
  | Undef	-> "undef"
  | Unit	-> "unit"
  | Byte	-> "byte"
  | Int		-> "int"
  | Bool	-> "bool"
  | Fun(typs)	->
     let typs = List.map pprint typs in
     let typs = String.concat "." typs in
     Printf.sprintf "(%s)" typs

