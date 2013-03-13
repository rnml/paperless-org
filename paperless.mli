open Core.Std

module Item : sig
  type t = {
    name : string;
    note : string option;
    read_only : bool;
    completed : bool;
    date_completed : string option;
  } with sexp
end

module List : sig
  type t = {
    name : string;
    icon_name : string;
    is_checklist : bool;
    items_to_top : bool;
    include_in_badge_count : bool;
    list_display_order : int;
    items : Item.t list;
  } with sexp
end

type t with sexp

module Xml : sig
  val load : string -> t
  (* val save : t -> string -> unit *)
end

(*
module Org : sig
  val load : string -> t
  val save : t -> string -> unit
end
*)

val create : List.t list -> t

val iter : t -> f:(List.t -> unit) -> unit
