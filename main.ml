open Core.Std

module X = Paperless

let dir = "/home/nathanml/paperless"

let pull_command =
  Command.basic ~summary:"pull from paperless lists to org file"
    Command.Spec.(empty +> anon ("ORG-FILE" %: file))
    (fun file () ->
      let p = Paperless.Xml.load dir in
      Paperless.Org.save p file)

let push_command =
  Command.basic ~summary:"push from org file to paperless lists"
    Command.Spec.(empty +> anon ("ORG-FILE" %: file))
    (fun file () ->
      let p = Paperless.Org.load file in
      Paperless.Xml.save p dir)

let parse_command =
  Command.basic ~summary:"parse org file and render as a sexp"
    Command.Spec.(empty +> anon ("ORG-FILE" %: file))
    (fun file () ->
      Org.load file
      |! Org.sexp_of_t
      |! Sexp.to_string_hum
      |! print_endline)

let normalize_command =
  Command.basic ~summary:"normalize names in paperless lists"
    Command.Spec.(empty +> anon ("ORG-FILE" %: file))
    (fun file () ->
      let p = Paperless.Org.load file in
      let p = Paperless.normalize p in
      Paperless.Org.save p file)

let command =
  Command.group ~summary:"convert paperless lists between XML and Org-mode"
    [
      ("pull", pull_command);
      ("push", push_command);
      ("parse", parse_command);
      ("normalize", normalize_command);
    ]

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
