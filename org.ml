open Core.Std

type t = {
  note : string option;
  elts : elt list;
}

and elt = {
  name : string;
  tags : string list;
  properties : string String.Map.t;
  data : t;
}
with sexp

let to_string _ = assert false
let of_string _ = assert false
