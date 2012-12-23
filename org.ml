open Core.Std

type t = node list
 and node = Node of string * t
with sexp

let of_string text =
  String.split ~on:'\n' text
  |! List.map ~f:(fun line ->
    match String.chop_prefix line ~prefix:"* " with
    | None -> (false, line)
    | Some line -> (true, line))
  |! List.group ~break:(fun header _ -> header)
