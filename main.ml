open Core.Std

module Xml = Simple_xml

type xml = Xml.t =
  | Element of (string * (string * string) list * xml list)
  | PCData of string

module type Xml_conv = sig
  type t
  val of_xml : Xml.t -> t
  val to_xml : t -> Xml.t
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
    type t with sexp
    include Xml_conv with type t := t
  end
end = struct

  module Pstring = struct
    type t = string with sexp
    let to_xml x = Xml.PCData x
    let of_xml = function
      | PCData x -> x
      | xml -> xml_conv_fail "pstring" xml
  end

  module Pbool = struct
    type t = bool with sexp
    let to_xml t = PCData (if t then "YES" else "NO")
    let of_xml = function
      | PCData "YES" -> true
      | PCData "NO" -> false
      | xml -> xml_conv_fail "pbool" xml
  end

  module Pint = struct
    type t = int with sexp
    let to_xml t = PCData (Int.to_string t)
    let of_xml = function
      | Element _ as xml -> xml_conv_fail "pbool" xml
      | PCData i as xml ->
        try Int.of_string i
        with _ -> xml_conv_fail "pbool" xml
  end

  module Item = struct
    type t = Xml.t with sexp
    let to_xml = Fn.id
    let of_xml = Fn.id
  end

  module List = struct
    type t = {
      name : string;
      icon_name : string;
      is_checklist : bool;
      items_to_top : bool;
      include_in_badge_count : bool;
      list_display_order : int;
      items : Item.t list;
    } with sexp

    let of_xml = function
      | Element ("list", [], (
          Element ("listName", [], [name]) ::
          Element ("listIconName", [], [icon_name]) ::
          Element ("isChecklist", [], [is_checklist]) ::
          Element ("itemsToTop", [], [items_to_top]) ::
          Element ("includeInBadgeCount", [], [include_in_badge_count]) ::
          Element ("listDisplayOrder", [], [list_display_order]) ::
          items
        )) -> {
          name = Pstring.of_xml name;
          icon_name = Pstring.of_xml icon_name;
          is_checklist = Pbool.of_xml is_checklist;
          items_to_top = Pbool.of_xml items_to_top;
          include_in_badge_count = Pbool.of_xml include_in_badge_count;
          list_display_order = Pint.of_xml list_display_order;
          items = List.map ~f:Item.of_xml items;
        }
      | xml -> xml_conv_fail "list" xml

    let to_xml {name; icon_name; is_checklist; items_to_top;
        include_in_badge_count; list_display_order; items} =
      let name = Pstring.to_xml name in
      let icon_name = Pstring.to_xml icon_name in
      let is_checklist = Pbool.to_xml is_checklist in
      let items_to_top = Pbool.to_xml items_to_top in
      let include_in_badge_count = Pbool.to_xml include_in_badge_count in
      let list_display_order = Pint.to_xml list_display_order in
      let items = List.map ~f:Item.to_xml items in
      Element ("list", [], (
        Element ("listName", [], [name]) ::
        Element ("listIconName", [], [icon_name]) ::
        Element ("isChecklist", [], [is_checklist]) ::
        Element ("itemsToTop", [], [items_to_top]) ::
        Element ("includeInBadgeCount", [], [include_in_badge_count]) ::
        Element ("listDisplayOrder", [], [list_display_order]) ::
        items
      ))
  end
end

let dir = "/home/nathanml/paperless-lists"
let files = [
  "groceries.xml";
  "bible.xml";
  "emacs.xml";
  "jentry.xml";
  "kid-songs.xml";
  "moss.xml";
  "new-years-resolutions.xml";
  "notes.xml";
  "org.xml";
  "someday.xml";
  "this-week.xml";
  "today.xml";
]

let () =
  Exn.handle_uncaught ~exit:true (fun () ->
    List.iter files ~f:(fun file ->
      Xml.load (dir ^ "/" ^ file)
      |! Paperless.List.of_xml
      |! Paperless.List.sexp_of_t
      |! Sexp.to_string_hum
      |! print_endline))

