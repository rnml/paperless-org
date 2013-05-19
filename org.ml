open Core.Std

module Level = struct
  type t =
  | Infinity
  | Num of int (* > 0 *)
  with compare, sexp
  let equal t1 t2 = compare t1 t2 = 0
  let pred = function
    | Infinity -> Infinity
    | Num n -> assert (n > 0); Num (n - 1)
  let add t k =
    match t with
    | Infinity -> Infinity
    | Num n -> assert (n > 0); Num (n + k)
end

module Line = struct
  type t = Level.t * string with sexp

  let to_string : t -> string = function
    | (Level.Infinity, s) -> s
    | (Level.Num n, s) -> String.make n '*' ^ " " ^ s

  let of_string s : t =
    match String.lfindi s ~f:(fun _ c -> not (Char.equal c '*')) with
    | Some n when n > 0 && Char.equal s.[n] ' ' ->
      (Level.Num n, String.sub s ~pos:(n + 1) ~len:(String.length s - n - 1))
    | _ -> (Level.Infinity, s)

end

type t = {
  preamble : string list;
  items : item list;
}
and item = {
  completed : bool;
  header : string;
  tags : string list;
  properties : (string, string) List.Assoc.t;
  body : t;
}
  with sexp

let to_lines t =
  let rec aux n {preamble; items} =
    List.map preamble ~f:(fun line ->
      let (level, text) = Line.of_string line in
      Line.to_string (Level.add level n, text))
    @ List.concat_map items
      ~f:(fun {completed; header; tags; properties; body} ->
        let header =
          if completed then "DONE " ^ header else header
        in
        let header =
          if List.is_empty tags then header else
            header
            ^ "        :"
            ^ String.concat tags ~sep:":"
            ^ ":"
        in
        Line.to_string (Level.Num n, header) ::
          List.map properties ~f:(fun (key, value) ->
            String.make (n + 1) ' ' ^ ":" ^ key ^ ": " ^ value)
        @ aux (n + 1) body)
  in
  aux 1 t

let of_lines ls =
  let rec aux ls =
    let groups =
      List.group ls ~break:(fun _ (n2, _) ->
        match n2 with Level.Num 1 -> true | _ -> false)
    in
    let block = function
      | (Level.Num 1, header) :: elts ->
        let body =
          aux (List.map elts ~f:(fun (n, x) -> (Level.pred n, x)))
        in
        let (properties, body) =
          let parse line =
            let line = String.strip line in
            match
              String.split line ~on:' '
              |! List.filter ~f:(fun token ->
                not (String.is_empty token))
            with
            | key :: value ->
              let value = String.concat ~sep:" " value in
              let open Option.Monad_infix in
              String.chop_prefix key ~prefix:":"
              >>= fun key ->
              String.chop_suffix key ~suffix:":"
              >>= fun key ->
              Some (key, value)
            | _ -> None
          in
          let (properties, preamble) =
            List.split_while body.preamble ~f:(fun line ->
              match parse line with
              | Some _ -> true
              | None -> false
            )
          in
          let properties =
            List.map properties ~f:(fun line ->
              match parse line with
              | None -> assert false
              | Some x -> x)
          in
          (properties, {body with preamble})
        in
        let (completed, header) =
          match String.chop_prefix header ~prefix:"DONE " with
          | None -> (false, header)
          | Some header -> (true, header)
        in
        let (header, tags) =
          match String.chop_suffix ~suffix:":" header with
          | None -> (header, [])
          | Some header ->
            let rec aux header tags =
              match String.rsplit2 header ~on:':' with
              | None -> (header, tags)
              | Some (rest, tag) ->
                if
                  String.exists tag ~f:(function
                  | ' ' | '-' -> true
                  | _ -> false)
                then
                  (header, tags)
                else
                  aux rest (tag :: tags)
          in
          aux header []
        in
        let header = String.strip header in
        {completed; header; tags; properties; body}
      | _ -> assert false
    in
    match groups with
    | [] ->
      { preamble = [];
        items = [] }
    | ((Level.Num 1, _) :: _ as group) :: groups ->
      { preamble = [];
        items = List.map ~f:block (group :: groups) }
    | group :: groups ->
      { preamble = List.map ~f:Line.to_string group;
        items = List.map ~f:block groups }
  in
  aux (List.map ls ~f:Line.of_string)

let of_string s = String.split ~on:'\n' s |! of_lines
let to_string t = to_lines t |! String.concat ~sep:"\n"

let load file = In_channel.read_lines file |! of_lines
let save t file = to_lines t |! Out_channel.write_lines file

let item_to_lines item = to_lines {preamble = []; items = [item]}
