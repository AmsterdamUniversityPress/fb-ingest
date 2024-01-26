open Cmdliner

let ($) = Term.($)

let arg_infile =
  let doc = "The path to the CSV file." in
  Arg.(required @@ pos ~rev:true 0 (some string) None @@ info [] ~docv:"csv-file" ~doc)

let arg_conf =
  let doc = "Skip all validation of column data." in
  Arg.(value @@ flag @@ info ["n"; "no-validate"] ~doc)

let cmd f =
  let bin' =
    let clean' = Filename.(remove_extension % basename) in
    Sys.argv.(0) |> clean' in
  let doc' = "Ingest and validate fb CSV data." in
  let terms' = Term.const f $ arg_conf $ arg_infile in
  let info' = Cmd.info bin' ~doc:doc' ~exits:Cmd.Exit.defaults in
  Cmd.v info' terms'

let parse cb =
  exit (Cmd.eval (cmd cb))
