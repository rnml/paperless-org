open Core.Std

type t = node list
 and node = Node of string * t
with sexp

val of_string : string -> t
