open Core.Std

type xml = Xml.xml =
  | Element of (string * (string * string) list * xml list)
  | PCData of string
with sexp

module type Xml_conv = sig
  type t
  val of_xml : xml -> t
  val to_xml : t -> xml
end

module Simple_xml : sig
  type t = xml with sexp
  include Xml_conv with type t := t
  val load : string -> t
  val save : t -> string -> unit
end = struct
  include Simple_xml
  let of_xml = Fn.id
  let to_xml = Fn.id
end

let xml_conv_fail name xml =
  Error.raise
    (Error.create (sprintf "bad %s" name) xml Simple_xml.sexp_of_t)

let xmls_conv_fail name xmls =
  Error.raise
    (Error.create (sprintf "bad %s" name) xmls
      (List.sexp_of_t Simple_xml.sexp_of_t))

module Pstring : sig
  type t = string with sexp
  include Xml_conv with type t := t
end = struct
  type t = string with sexp
  let to_xml x = PCData x
  let of_xml = function
    | PCData x -> x
    | xml -> xml_conv_fail "pstring" xml
end

module Pbool : sig
  type t = bool with sexp
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
  type t = int with sexp
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
  with sexp
  include Xml_conv with type t := t
  val to_org : t -> Org.item
  val of_org : Org.item -> t
end = struct
  type t = {
    name : string;
    note : string sexp_option;
    read_only : sexp_bool;
    completed : sexp_bool;
    date_completed : string sexp_option;
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
      Element ("dateCompleted", [], date_completed);
      Element ("itemName", [], [name]);
      Element ("itemNote", [], note);
      Element ("itemReadOnly", [], [read_only]);
      Element ("itemCompleted", [], [completed]);
    ])

  let of_xml xml =
    let fail () = xml_conv_fail "Item.t" xml in
    match xml with
    | Element ("item", [], elts) ->
      begin
        let alist =
          List.filter_map elts ~f:(function
          | Element (key, [], value) -> Some (key, value)
          | _ -> None)
        in
        if List.length alist = List.length elts then
          let result =
            match String.Map.of_alist alist with
            | `Duplicate_key _ -> None
            | `Ok elts ->
              let open Option.Monad_infix in
              let only_one = function [x] -> Some x | _ -> None in
              Map.find elts "dateCompleted"
              >>= fun date_completed ->
              (Map.find elts "itemName" >>= only_one)
              >>= fun name ->
              Map.find elts "itemNote"
              >>= fun note ->
              (Map.find elts "itemReadOnly" >>= only_one)
              >>= fun read_only ->
              (Map.find elts "itemCompleted" >>= only_one)
              >>= fun completed ->
              Some {
                name = Pstring.of_xml name;
                note = option_of_xml "Item.note" Pstring.of_xml note;
                read_only = Pbool.of_xml read_only;
                completed = Pbool.of_xml completed;
                date_completed =
                  option_of_xml "Item.date_completed"
                    Pstring.of_xml date_completed;
              }
          in
          match result with
          | None -> fail ()
          | Some x -> x
        else
          fail ()
      end
    | _ -> fail ()

  let to_org {name; note; read_only; completed; date_completed} =
    let tags =
      List.filter_opt [
        if read_only then Some "read_only" else None;
      ]
    in
    { Org.
      header = name;
      completed;
      tags;
      properties =
        List.filter_opt [
          Option.map date_completed ~f:(fun date ->
            ("completed", date));
        ];
      body =
        { Org.
          preamble =
            Option.to_list note
            |> List.concat_map ~f:(String.split ~on:'\n');
          items = []
        };
    }

  let of_org {Org.header; completed; tags; properties; body} =
    let name = header in
    let (known_tags, other_tags) =
      List.partition_map tags ~f:(fun tag ->
        match tag with
        | "read_only" -> `Fst tag
        | _ -> `Snd tag)
    in
    let name =
      if List.is_empty other_tags then name else
        name ^ "      :" ^ String.concat ~sep:":" other_tags ^ ":"
    in
    let read_only = List.mem known_tags "read_only" in
    let (known_properties, other_properties) =
      List.partition_map properties ~f:(fun (key, value) ->
        match key with
        | "completed" -> `Fst (key, value)
        | _ -> `Snd (key, value)
      )
    in
    let date_completed = List.Assoc.find known_properties "completed" in
    let note =
      (List.map other_properties ~f:(fun (key, value) ->
        ":" ^ key ^ ": " ^ value)
       @ body.Org.preamble
       @ List.concat_map body.Org.items ~f:Org.item_to_lines
      )
    in
    let note =
      if List.is_empty note then None else Some (String.concat ~sep:"\n" note)
    in
    {name; note; read_only; completed; date_completed}

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
  } with fields, sexp
  include Xml_conv with type t := t
  val to_org : t -> Org.item
  val of_org : Org.item -> t
  val normalize : t -> t
end = struct
  type t = {
    name : string;
    icon_name : string;
    is_checklist : sexp_bool;
    items_to_top : sexp_bool;
    include_in_badge_count : sexp_bool;
    list_display_order : int;
    items : Item.t list;
  } with sexp, fields

  let prelude = "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet type=\"text/css\" \
href=\"http://crushapps.com/paperless/xml_style/checklist.css\"?>"

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

  let to_org { name; icon_name; is_checklist; items_to_top;
               include_in_badge_count; list_display_order; items} =
    ignore list_display_order; (* implied *)
    let tags =
      List.filter_opt [
        if is_checklist then Some "checklist" else None;
        if include_in_badge_count then Some "count" else None;
      ]
    in
    { Org.
      header = name;
      completed = false;
      tags;
      properties = [
        ("icon_name", icon_name);
        ("items_to_top", Bool.to_string items_to_top);
      ];
      body =
        { Org.
          preamble = [];
          items = List.map items ~f:Item.to_org;
        };
    }

  let of_org ({Org.header; completed = _; tags; properties; body} as org) =
    let name = header in
    let (known_tags, other_tags) =
      List.partition_map tags ~f:(fun tag ->
        match tag with
        | "checklist" | "count" -> `Fst tag
        | _ -> `Snd tag)
    in
    let name =
      if List.is_empty other_tags then name else
        name ^ "      :" ^ String.concat ~sep:":" other_tags ^ ":"
    in
    let is_checklist = List.mem known_tags "checklist" in
    let include_in_badge_count = List.mem known_tags "count" in
    let (known_properties, other_properties) =
      List.partition_map properties ~f:(fun (key, value) ->
        match key with
        | "icon_name" | "items_to_top" -> `Fst (key, value)
        | _ -> `Snd (key, value)
      )
    in
    ignore other_properties;
    let items_to_top =
      match
        List.Assoc.find known_properties "items_to_top"
      with
      | Some b -> Bool.of_string b
      | None ->
        failwiths "missing items_to_top" org
          <:sexp_of< Org.item >>
    in
    let icon_name  =
      List.Assoc.find_exn known_properties "icon_name"
    in
    let items = List.map body.Org.items ~f:Item.of_org in
    { name; icon_name; is_checklist; items_to_top;
      include_in_badge_count; list_display_order = 0; items}

  let normalize t =
    let norm s = String.lowercase s |> String.tr ~target:'_' ~replacement:'-' in
    {t with name = norm t.name}
end

module Index : sig
  type t = string list with sexp
  val load : string -> t
  val save : t -> string -> unit
end = struct

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
        |> String.split ~on:'\n'
        |> List.map ~f:(fun line ->
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

  let save t path =
    Out_channel.with_file path ~f:(fun cout ->
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
  List.iter plists ~f:(fun plist ->
add t plist);
  t

let xml_load dir =
  let index = Index.load (dir ^ "/index.plist") in
  List.map index ~f:(fun file ->
    let xml = sprintf "%s/%s.xml" dir file |> Simple_xml.load in
    Plist.of_xml xml)
  |> create

let index t = List.map ~f:Plist.name (Hq.to_list t)

let xml_save t dir =
  Index.save (index t) (dir ^ "/index.plist");
  Hq.iteri t ~f:(fun ~key:name ~data:plist ->
    let path = sprintf "%s/%s.xml" dir name in
    Simple_xml.save (Plist.to_xml plist) path)

let xml_save new_t dir =
  let old_t = xml_load dir in
  let keys t = Hq.keys t |> String.Set.of_list in
  let new_keys = keys new_t in
  let old_keys = keys old_t in
  xml_save new_t dir;
  Set.iter (Set.diff old_keys new_keys) ~f:(fun name ->
    Unix.unlink (dir ^/ name ^ ".xml"))

let sexp_of_t t = <:sexp_of< Plist.t list >> (Hq.to_list t)
let t_of_sexp s = create (<:of_sexp< Plist.t list >> s)

let org_conv_fail name org =
  Error.raise
    (Error.create (sprintf "bad %s" name) org Org.sexp_of_t)

let org_save t file =
  let org =
    { Org.
      preamble = [];
      items = List.map (Hq.to_list t) ~f:Plist.to_org;
    }
  in
  Org.save org file

let org_load file =
  let org = Org.load file in
  create (List.map ~f:Plist.of_org org.Org.items)

let normalize t =
  Hq.to_list t
  |> List.map ~f:Plist.normalize
  |> create

module List = Plist

module type Format = sig
  val load : string -> t
  val save : t -> string -> unit
end

module Xml = struct
  let load = xml_load
  let save = xml_save
end

module Org = struct
  let load = org_load
  let save = org_save
end
