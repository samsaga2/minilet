type t = string * int


let nextsym = ref 0
let symbol2id : (string, int) Hashtbl.t = Hashtbl.create 64
let id2symbol : (int, string) Hashtbl.t = Hashtbl.create 64


let symbol name =
  try
    let id = Hashtbl.find symbol2id name in
    (name, id)
  with Not_found ->
    let id = !nextsym in
    incr nextsym;
    Hashtbl.add symbol2id name id;
    Hashtbl.add id2symbol id name;
    (name, id)


let new_symbol () =
  let name = "#"^(string_of_int !nextsym) in
  incr nextsym;
  symbol name


let name (name,_) = name


let id (_,id) = id


let eq (_,id1) (_,id2) =
  id1==id2


let get id =
  Hashtbl.find id2symbol id
