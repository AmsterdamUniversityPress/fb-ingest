let () =
  let cb skip_validate infile' = Fb_ingest.go Fmt.stdout infile' ~skip_validate in
  Args.parse cb
