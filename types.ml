type t =
  | Unit
  | Byte
  | Int
  | Bool
  | Fun of t*t
