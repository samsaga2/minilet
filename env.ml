module Env = Map.Make(
                 struct
                   type t = int
                   let compare = Pervasives.compare
                 end)


type 'a table = 'a Env.t


let empty = Env.empty


let put symtable (_,id) value =
  Env.add id value symtable


let put_pair symtable (key,value) =
  put symtable key value


let get symtable (_,id) =
  try
    Some (Env.find id symtable)
  with Not_found ->
    None


let iter fn symtable =
  Env.iter fn symtable
