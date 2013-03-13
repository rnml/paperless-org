open Core.Std

(*
type t = node list
 and node = Node of string * t
with sexp

let rec of_lines lines =
  List.map lines ~f:(fun line ->
    match String.chop_prefix line ~prefix:"* " with
    | None -> (false, line)
    | Some line -> (true, line))
  |! List.group ~break:(fun header _ -> fst header)
  |! List.map ~f:(function
    | [] -> assert false
    |

let of_string text = String.split ~on:'\n' text |! of_lines

List.map ~f:snd)
*)
