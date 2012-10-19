open Core.Std

type t = Xml.xml =
  | Element of (string * (string * string) list * t list)
  | PCData of string
with sexp

let load path =
  let module Parser = XmlParser in
  Parser.parse (Parser.make ()) (Parser.SFile path)

let of_xml = Fn.id
let to_xml = Fn.id
