open Core.Std

type t = Xml.xml =
  | Element of (string * (string * string) list * t list)
  | PCData of string
with sexp

val load : string -> t
val save : t -> string -> unit
