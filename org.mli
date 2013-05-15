open Core.Std

type t = {
  preamble : string list;
  items : item list;
}
and item = {
  header : string;
  properties : (string, string) List.Assoc.t;
  body : t;
}
  with sexp

include Stringable with type t := t

val load : string -> t
val save : t -> string -> unit
