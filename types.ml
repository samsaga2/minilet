type t =
  | Unit
  | Nil
  | Byte
  | Int
  | Bool
  | Fun of t*t
