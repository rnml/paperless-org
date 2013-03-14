open Core.Std

module X = Paperless

let dir = "/home/nathanml/paperless-lists"

let main () =
  Paperless.Xml.load dir
  |! Paperless.sexp_of_t
  |! Sexp.to_string_hum
  |! print_endline

let main () =
  Sexp.load_sexp_conv_exn "foo-org.sexp" Org.t_of_sexp
  |! Org.to_string
  |! print_endline

let main () =
  Org.load "yo.org"
  (* |! Org.sexp_of_t |! Sexp.to_string_hum *)
  |! Org.to_string
  |! print_string

let () = Exn.handle_uncaught ~exit:true main
