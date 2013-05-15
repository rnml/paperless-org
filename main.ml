open Core.Std

module X = Paperless

let dir = "/home/nathanml/paperless-lists"

let main () =
  Org.load "foo.org"
  |! Org.to_string
  |! print_string

let main () =
  let p = Paperless.Xml.load dir in
  Paperless.Xml.save p dir

let () = Exn.handle_uncaught ~exit:true main
