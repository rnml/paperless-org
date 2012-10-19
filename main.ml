open Core.Std

module Xml : sig
  type t =
    | Element of (string * (string * string) list * t list)
    | PCData of string
  with sexp
  val load : string -> t
end = struct
  module Dtd    = Dtd
  module Xml    = Xml
  module Parser = XmlParser

  type t = Xml.xml =
    | Element of (string * (string * string) list * t list)
    | PCData of string
  with sexp

  let load path = Parser.parse (Parser.make ()) (Parser.SFile path)
end

module type Xml_conv = sig
  type t
  val of_xml : Xml.t -> t
  (*val to_xml : t -> Xml.t*)
end

let xml_conv_fail name xml =
  Error.raise
    (Error.create (sprintf "bad %s" name) xml Xml.sexp_of_t)

module Paperless : sig
  module Pbool : sig
    type t = bool with sexp
    include Xml_conv with type t := t
  end
  module List : sig
  (* type t with sexp
   * include Xml_conv with type t := t *)
  end
end = struct
  module Pbool = struct
    type t = bool with sexp
    let of_xml = function
      | Xml.PCData "YES" -> true
      | Xml.PCData "NO" -> false
      | xml -> xml_conv_fail "pbool" xml
  end
  module Item = struct
    type t
  end
  module List = struct
    type t = {
      name : string;
      icon_name : string;
      is_checklist : bool;
      items_to_top : bool;
      include_in_badge_count : bool;
      items : Item.t list;
    }
  end
end

let xml = Xml.load "/home/nathanml/paperless-lists/groceries.xml"

let () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Xml.sexp_of_t xml |! Sexp.to_string_hum |! print_endline)
