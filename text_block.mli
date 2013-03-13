type t (* two dimensional block of text *)

type valign = [`Top | `Bottom | `Center]
type halign = [`Left | `Right | `Center]

val text : ?align:halign -> string -> t

(* assumes width and height are non-negative *)
val span : ?ch:char -> width:int -> height:int -> unit -> t

val hstrut : int -> t (* zero height *)
val vstrut : int -> t (* zero width *)

(* horizontal and vertical concatenation with alignment *)
val hcat : ?align:valign -> ?sep:t -> t list -> t
val vcat : ?align:halign -> ?sep:t -> t list -> t

(* the empty block. a left and right unit to both hcat and vcat *)
val nil : t

(* text block dimensions *)
val width  : t -> int
val height : t -> int

(* add the specified amount of horizontal or vertical padding *)
val hpad : t -> align:halign -> int -> t
val vpad : t -> align:valign -> int -> t

val valign : valign -> t list -> t list
val halign : halign -> t list -> t list

val render : t -> string
