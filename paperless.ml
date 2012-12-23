open Core.Std

type xml = Xml.xml =
  | Element of (string * (string * string) list * xml list)
  | PCData of string
with sexp

module type Xml_conv = sig
  type t with sexp
  val of_xml : xml -> t
  val to_xml : t -> xml
end

module Xml : sig
  type t = xml
  include Xml_conv with type t := t
  val load : string -> t
end = struct

  type t = xml with sexp

  let load path =
    let module Parser = XmlParser in
    Parser.parse (Parser.make ()) (Parser.SFile path)

  let of_xml = Fn.id
  let to_xml = Fn.id
end

let xml_conv_fail name xml =
  Error.raise
    (Error.create (sprintf "bad %s" name) xml Xml.sexp_of_t)

let xmls_conv_fail name xmls =
  Error.raise
    (Error.create (sprintf "bad %s" name) xmls
      (List.sexp_of_t Xml.sexp_of_t))

module Pstring : sig
  type t = string
  include Xml_conv with type t := t
end = struct
  type t = string with sexp
  let to_xml x = PCData x
  let of_xml = function
    | PCData x -> x
    | xml -> xml_conv_fail "pstring" xml
end

module Pbool : sig
  type t = bool
  include Xml_conv with type t := t
end = struct
  type t = bool with sexp
  let to_xml t = PCData (if t then "YES" else "NO")
  let of_xml = function
    | PCData "YES" -> true
    | PCData "NO" -> false
    | xml -> xml_conv_fail "Pbool" xml
end

module Pint : sig
  type t = int
  include Xml_conv with type t := t
end = struct
  type t = int with sexp
  let to_xml t = PCData (Int.to_string t)
  let of_xml = function
    | Element _ as xml -> xml_conv_fail "Pbool" xml
    | PCData i as xml ->
      try Int.of_string i
      with _ -> xml_conv_fail "Pbool" xml
end

let option_to_xml to_xml = function
  | None -> []
  | Some x -> [to_xml x]

let option_of_xml name of_xml = function
  | [] -> None
  | [x] -> Some (of_xml x)
  | xmls -> xmls_conv_fail name xmls

module Item : sig
  type t = {
    name : string;
    note : string option;
    read_only : bool;
    completed : bool;
    date_completed : string option;
  }
  include Xml_conv with type t := t
end = struct
  type t = {
    name : string;
    note : string option;
    read_only : bool;
    completed : bool;
    date_completed : string option;
  } with sexp

  let to_xml t =
    let name = Pstring.to_xml t.name in
    let note = option_to_xml Pstring.to_xml t.note in
    let read_only = Pbool.to_xml t.read_only in
    let completed = Pbool.to_xml t.completed in
    let date_completed =
      option_to_xml Pstring.to_xml t.date_completed
    in
    Element ("item", [], [
      Element ("itemName", [], [name]);
      Element ("itemNote", [], note);
      Element ("itemReadOnly", [], [read_only]);
      Element ("itemCompleted", [], [completed]);
      Element ("dateCompleted", [], date_completed);
    ])

  let of_xml = function
    | Element ("item", [], [
        Element ("itemName", [], [name]);
        Element ("itemNote", [], note);
        Element ("itemReadOnly", [], [read_only]);
        Element ("itemCompleted", [], [completed]);
        Element ("dateCompleted", [], date_completed);
      ]) -> {
        name = Pstring.of_xml name;
        note = option_of_xml "Item.note" Pstring.of_xml note;
        read_only = Pbool.of_xml read_only;
        completed = Pbool.of_xml completed;
        date_completed =
          option_of_xml "Item.date_completed"
            Pstring.of_xml date_completed;
      }
    | xml -> xml_conv_fail "Item.t" xml
end

module Plist : sig
  type t = {
    name : string;
    icon_name : string;
    is_checklist : bool;
    items_to_top : bool;
    include_in_badge_count : bool;
    list_display_order : int;
    items : Item.t list;
  }
  include Xml_conv with type t := t
end = struct
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
        Element
          ("listName", [], [name]) ::
        Element
          ("listIconName", [], [icon_name]) ::
        Element
          ("isChecklist", [], [is_checklist]) ::
        Element
          ("itemsToTop", [], [items_to_top]) ::
        Element
          ("includeInBadgeCount", [], [include_in_badge_count]) ::
        Element
          ("listDisplayOrder", [], [list_display_order]) ::
        items
      )) -> {
        name = Pstring.of_xml name;
        icon_name = Pstring.of_xml icon_name;
        is_checklist = Pbool.of_xml is_checklist;
        items_to_top = Pbool.of_xml items_to_top;
        include_in_badge_count =
          Pbool.of_xml include_in_badge_count;
        list_display_order = Pint.of_xml list_display_order;
        items = List.map ~f:Item.of_xml items;
      }
    | xml -> xml_conv_fail "List.t" xml

  let to_xml {name; icon_name; is_checklist; items_to_top;
      include_in_badge_count; list_display_order; items} =
    let name = Pstring.to_xml name in
    let icon_name = Pstring.to_xml icon_name in
    let is_checklist = Pbool.to_xml is_checklist in
    let items_to_top = Pbool.to_xml items_to_top in
    let include_in_badge_count =
      Pbool.to_xml include_in_badge_count
    in
    let list_display_order = Pint.to_xml list_display_order in
    let items = List.map ~f:Item.to_xml items in
    Element ("list", [], (
      Element
        ("listName", [], [name]) ::
      Element
        ("listIconName", [], [icon_name]) ::
      Element
        ("isChecklist", [], [is_checklist]) ::
      Element
        ("itemsToTop", [], [items_to_top]) ::
      Element
        ("includeInBadgeCount", [], [include_in_badge_count]) ::
      Element
        ("listDisplayOrder", [], [list_display_order]) ::
      items
    ))
end

module Index = struct

  let prelude = "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<array>"

  let postlude = "\
</array>
</plist>"

  type t = string list with sexp

  let load path : t =
    let contents = In_channel.read_all path in
    let contents = String.strip contents in
    match String.chop_prefix ~prefix:prelude contents with
    | None ->
      Error.raise
        (Error.create "Index: bad prelude"
          (prelude, String.prefix contents (String.length prelude))
          <:sexp_of<string * string>>)
    | Some contents ->
      match String.chop_suffix ~suffix:postlude contents with
      | None ->
        failwithf "Index: bad postlude %S"
          (String.suffix contents (String.length postlude)) ()
      | Some contents ->
        String.strip contents
        |! String.split ~on:'\n'
        |! List.map ~f:(fun line ->
          let line = String.strip line in
          match String.chop_prefix ~prefix:"<string>" line with
          | None ->
            failwithf "Index line: bad prefix %S"
              (String.prefix line (String.length "<string>")) ()
          | Some line ->
            match String.chop_suffix ~suffix:"</string>" line with
            | None ->
              failwithf "Index line: bad suffix %S"
                (String.suffix line (String.length "</string>")) ()
            | Some line -> line
          )

  let save path t =
    Out_channel.with_file path (fun cout ->
      Out_channel.output_string cout prelude;
      Out_channel.newline cout;
      List.iter t ~f:(fun name ->
        Out_channel.output_string cout "\t<string>";
        Out_channel.output_string cout name;
        Out_channel.output_string cout "</string>";
        Out_channel.newline cout;
      );
      Out_channel.output_string cout postlude;
      Out_channel.newline cout;
    )

end

module Hq = String.Hash_queue

type t = Plist.t Hq.t

let add t plist =
  let size = Hq.length t in
  let name = plist.Plist.name in
  let plist = {plist with Plist.list_display_order = size} in
  match Hq.enqueue t name plist with
  | `Ok -> ()
  | `Key_already_present ->
    Error.raise
      (Error.create "two lists named" name String.sexp_of_t)

let create plists =
  let open Result.Monad_infix in
  let t = Hq.create () in
  List.iter plists ~f:(fun plist -> add t plist);
  t

let load dir =
  let index = Index.load (dir ^ "/index.plist") in
  List.map index ~f:(fun file ->
    sprintf "%s/%s.xml" dir file |! Xml.load |! Plist.of_xml)
  |! create

let iter = Hq.iter
let fold = Hq.fold

let sexp_of_t t = List.sexp_of_t Plist.sexp_of_t (Hq.to_list t)
let t_of_sexp s = create (List.t_of_sexp Plist.t_of_sexp s)

module List = Plist
