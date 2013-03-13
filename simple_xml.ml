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
    Out_channel.output_string cout (Xml.to_string_fmt t))
