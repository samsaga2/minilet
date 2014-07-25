type t =
  | Undef
  | Unit
  | Byte
  | Int
  | Bool
  | Fun of t*t

let rec pprint = function
  | Undef	-> "undef"
  | Unit	-> "unit"
  | Byte	-> "byte"
  | Int		-> "int"
  | Bool	-> "bool"
  | Fun(t1,t2)	-> (pprint t1)^"."^(pprint t2)
