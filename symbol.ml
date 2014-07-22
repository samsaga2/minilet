type t = string * int

let nextsym = ref 0
let nextsym = ref 0
let nextlbl = ref 0

let hashtable : (string, int) Hashtbl.t = Hashtbl.create 64

let symbol name =
  try
    let id = Hashtbl.find hashtable name in
    (name, id)
  with Not_found ->
    let id = !nextsym in
    incr nextsym;
    Hashtbl.add hashtable name id;
    (name, id)

let new_symbol () =
  let name = "#"^(string_of_int !nextsym) in
  incr nextsym;
  symbol name

let new_label () =
  let name = "_"^(string_of_int !nextlbl) in
  incr nextlbl;
  symbol name

let name (s, _) = s

module SymbolTable = Map.Make(
                         struct
                           type t = int
                           let compare = Pervasives.compare
                         end)

type 'a table = 'a SymbolTable.t

let empty = SymbolTable.empty

let put symtable (_,id) value =
  SymbolTable.add id value symtable

let put_pair symtable (key,value) =
  put symtable key value

let get symtable (_,id) =
  try
    Some (SymbolTable.find id symtable)
  with Not_found ->
    None
