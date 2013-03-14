open Core.Std

type t = {note : string option; elts : elt list}
and elt = {name : string; tags : string list; properties : string String.Map.t; data : t}
with sexp

let vcat txts = Text_block.vcat ~align:`Left txts
let hcat txts = Text_block.hcat ~align:`Top  txts

let rec to_text n t =
  let sep = Text_block.text (String.make n ' ') in
  let note =
    match t.note with
    | None -> Text_block.nil
    | Some text -> Text_block.text text
  in
  let elts = List.map t.elts ~f:(fun elt -> elt_to_text n elt) in
  vcat (hcat [sep; note] :: elts)

and elt_to_text n e =
  let header =
    Text_block.text (String.make n '*' ^ " " ^ e.name ^ begin
      if List.is_empty e.tags then "" else
        "   :" ^ String.concat ~sep:":" e.tags ^ ":"
    end)
  in
  let properties =
    List.map (String.Map.to_alist e.properties)
      ~f:(fun (key, value) -> Text_block.text (":" ^ key ^ ":   " ^ value))
  in
  vcat [
    header;
    hcat [
      Text_block.text (String.make (n + 1) ' ');
      vcat properties;
    ];
    to_text (n + 1) e.data;
  ]

let to_string t = Text_block.render (to_text 1 t)

let of_string x =
  let rec loop n lines : t =
    let prefix = String.make n '*' ^ " " in
    let lines =
      List.map lines ~f:(fun line ->
        match String.chop_prefix line ~prefix with
        | None -> `Line line
        | Some header -> `Header header)
    in
    let groups =
      List.group lines ~break:(fun _ line ->
        match line with
        | `Header _ -> true
        | `Line _ -> false)
    in
    let groups =
      List.map groups ~f:(function
      | [] -> assert false
      | (`Line _ :: _) as lines ->
        let lines =
          List.map lines ~f:(function `Line line -> line | `Header _ -> assert false)
        in
        let lines =
          let prefix = String.make n ' ' in
          List.map lines ~f:(fun line ->
            Option.value ~default:line (String.chop_prefix line ~prefix))
        in
        `Lines (String.concat lines ~sep:"\n")
      | (`Header h :: lines) ->
        let lines =
          List.map lines ~f:(function `Line line -> line | `Header _ -> assert false)
        in
        `With_header (h, lines))
    in
    let (note, whs) =
      match groups with
      | `Lines lines :: whs -> (Some lines, whs)
      | whs -> (None, whs)
    in
    let elts =
      List.map whs ~f:(function
      | `Lines _ -> assert false
      | `With_header (name, lines) ->
        let property line =
          let line = String.strip line in
          match String.split line ~on:':' with
          | "" :: key :: rest ->
            let rest = String.concat ~sep:":" rest in
            let rest = String.strip rest in
            Some (key, rest)
          | _ -> None
        in
        let is_prop line = Option.is_some (property line) in
        let properties =
          List.take_while lines ~f:is_prop
          |! List.filter_map ~f:property
          |! String.Map.of_alist_exn
        in
        let rest = List.drop_while lines ~f:is_prop in
        let data = loop (n + 1) rest in
        {name; tags = []; properties; data}
      )
    in
    {note; elts}
  in
  loop 1 (String.split x ~on:'\n')

let load path = In_channel.read_all path |! of_string
let save t path = Out_channel.write_all path ~data:(to_string t)
