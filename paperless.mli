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

val create : List.t list -> t
val iter : t -> f:(List.t -> unit) -> unit

module type Format = sig
  val load : string -> t
  val save : t -> string -> unit
end

val normalize : t -> t

module Xml : Format
module Org : Format

