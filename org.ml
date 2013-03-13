open Core.Std

type t = {note : string option; elts : elt list}
and elt = {name : string; tags : string list; properties : string String.Map.t; data : t}
with sexp

let vcat txts = Text_block.vcat ~align:`Left txts
let hcat txts = Text_block.hcat ~align:`Top  txts

let rec to_text n t =
  let note =
    match t.note with
    | None -> Text_block.nil
    | Some text -> Text_block.text text
  in
  let elts = List.map t.elts ~f:(fun elt -> elt_to_text n elt) in
  vcat (note :: elts)

and elt_to_text n e =
  let header =
    Text_block.text (String.make n '*' ^ " " ^ e.name ^ "   " ^ String.concat ~sep:":" e.tags)
  in
  let properties =
    List.map (String.Map.to_alist e.properties)
      ~f:(fun (key, value) -> Text_block.text (":" ^ key ^ ":   " ^ value))
  in
  vcat [
    header;
    hcat [
      Text_block.text (String.make (n + 1) ' ');
      vcat [
        vcat properties;
        to_text (n + 1) e.data;
      ];
    ];
  ]

let to_string t = Text_block.render (to_text 1 t)
let of_string _ = assert false
