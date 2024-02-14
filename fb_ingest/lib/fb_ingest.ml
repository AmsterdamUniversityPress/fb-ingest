let num_header_rows = 3

let skip_row _n = false
let die_on_fail = false

let to_json_string csv_reader ~skip_validate =
  let row_num = ref (-1) in
  (* --- skip row header *)
  let () = for _ = 1 to num_header_rows do
      let _ = Csv.next csv_reader in ()
    done in
  (* @todo it would be nice to move the "if else" clause
   * inside the fold function (but that's tricky with types) *)
  if skip_validate then
    csv_reader
    |> Csv.fold_left
      ~f:(fun acc record ->
          let () = row_num := !row_num + 1 in
          match Convert.process_row_no_validate !row_num record with
          | Ok record' ->
            (* @todo match seems redundant *)
            begin match acc with
              | acc' -> record' :: acc'
              | _ -> failwith "unexpected"
            end
          | Error (`Msg m) ->
            let () = Fmt.pr "%s" m in
            failwith "aborting")
      ~init: []
    |> (fun lst -> `List lst)
    |> Yojson.to_string
  else
    csv_reader
    |> Csv.fold_left
      ~f:(fun acc record ->
          let () = row_num := !row_num + 1 in
          if skip_row !row_num then acc else
            match Convert.process_row_validate !row_num record with
            (* --- reverses the order of the rows but is more efficient *)
            | Ok record' -> record' :: acc
            | Error (`Msg m) ->
              let () = Fmt.epr "%s (row=%d)@." m !row_num in
              if die_on_fail then failwith "aborting" else acc)
      ~init:[]
    |> Types.to_json_string

let go ppf file' ~skip_validate =
  let csv_reader = file' |> open_in |> Csv.of_channel in
  let () = Fmt.pf ppf "@[<v>%s@]@."
      (to_json_string csv_reader ~skip_validate) in
  Csv.close_in csv_reader
