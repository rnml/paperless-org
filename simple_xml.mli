open Core.Std

type t =
  | Element of (string * (string * string) list * t list)
  | PCData of string
with sexp

val load : string -> t

(* val save : string -> t -> unit *)

val of_xml : t -> t
val to_xml : t -> t
