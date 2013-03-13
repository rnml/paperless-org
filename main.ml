open Core.Std

module X = Paperless

let dir = "/home/nathanml/paperless-lists"

let main () =
  Paperless.Xml.load dir
  |! Paperless.sexp_of_t
  |! Sexp.to_string_hum
  |! print_endline

let () = Exn.handle_uncaught ~exit:true main
