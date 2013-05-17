open Core.Std

type t = {
  preamble : string list;
  items : item list;
}
and item = {
  completed : bool;
  header : string;
  tags : string list;
  properties : (string, string) List.Assoc.t;
  body : t;
}
  with sexp

include Stringable with type t := t

val item_to_lines : item -> string list

val load : string -> t
val save : t -> string -> unit
