type t = string * int


let nextsym = ref 0


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


let name (s, _) = s
