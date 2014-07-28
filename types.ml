type sym = Symbol.t

type t =
  | Error
  | Undef
  | Unit
  | Byte
  | Int
  | Bool
  | Fun of (t ref)*(t ref)

let rec pprint typ =
  match !typ with
  | Error       -> "error"
  | Undef	-> "undef"
  | Unit	-> "unit"
  | Byte	-> "byte"
  | Int		-> "int"
  | Bool	-> "bool"
  | Fun(t1,t2)	->
    let t1 = pprint t1 and t2 = pprint t2 in
    Printf.sprintf "(%s.%s)" t1 t2

