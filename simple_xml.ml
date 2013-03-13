open Core.Std

type t = Xml.xml =
  | Element of (string * (string * string) list * t list)
  | PCData of string
with sexp

let load path =
  let module Parser = XmlParser in
  Parser.parse (Parser.make ()) (Parser.SFile path)

let save t path =
  Out_channel.with_file path ~f:(fun cout ->
    Out_channel.print_string cout (Xml.to_string_fmt t)

let of_xml = Fn.id
let to_xml = Fn.id
