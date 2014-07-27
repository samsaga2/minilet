type sym = Symbol.t

type t =
  | Undef
  | Unit
  | Byte
  | Int
  | Bool
  | Fun of t ref list

let rec pprint typ =
  match !typ with
  | Undef	-> "(undef)"
  | Unit	-> "(unit)"
  | Byte	-> "(byte)"
  | Int		-> "(int)"
  | Bool	-> "(bool)"
  | Fun(typs)	->
     let typs = List.map pprint typs in
     "("^(String.concat "." typs)^")"
