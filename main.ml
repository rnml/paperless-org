open Core.Std

let pull_command =
  Command.basic ~summary:"pull from paperless lists to org file"
    Command.Spec.(
      empty
      +> anon ("ORG-FILE" %: file)
      +> flag "src" (required file) ~doc:"DIR paperless dir"
    )
    (fun file dir () ->
       let p = Paperless.Xml.load dir in
       let p = Paperless.normalize p in
       Paperless.Org.save p file)

let push_command =
  Command.basic ~summary:"push from org file to paperless lists"
    Command.Spec.(
      empty
      +> anon ("ORG-FILE" %: file)
      +> flag "dst" (required file) ~doc:"DIR paperless dir"
    )
    (fun file dir () ->
       let p = Paperless.Org.load file in
       let p = Paperless.normalize p in
       Paperless.Xml.save p dir)

let command =
  Command.group ~summary:"convert paperless lists between XML and Org-mode"
    [
      ("pull", pull_command);
      ("push", push_command);
    ]

let () = Command.run command
