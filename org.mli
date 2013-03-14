open Core.Std

type t = {note : string option; elts : elt list}
and elt = {name : string; tags : string list; properties : string String.Map.t; data : t}
with sexp

include Stringable with type t := t

val load : string -> t
val save : t -> string -> unit
