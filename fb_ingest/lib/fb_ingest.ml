let num_header_rows = 3

let to_json_validate csv_reader =
  let fondsen =
    let row_num = ref (-1) in
    let f acc record =
      let () = row_num := !row_num + 1 in
      match Convert.process_row_validate !row_num record with
      (* --- reverses the order of the rows but is more efficient *)
      | Ok record' -> record' :: acc
      | Error (`Msg m) ->
        let () = Fmt.pr "%s" m in
        failwith "aborting" in
    (* --- skip row header *)
    let () = for _ = 1 to num_header_rows do
        let _ = Csv.next csv_reader in ()
    done in
    Csv.fold_left ~f ~init:[] csv_reader in
  Types.to_json_string fondsen

let to_json_no_validate csv_reader =
  let x : Yojson.t =
    let row_num = ref (-1) in
    let f acc record =
      let () = row_num := !row_num + 1 in
      match Convert.process_row_no_validate !row_num record with
      | Ok record' ->
        begin match acc with
          | `List acc' -> `List (record' :: acc')
          | _ -> failwith "hahaha"
        end
      | Error (`Msg m) ->
        let () = Fmt.pr "%s" m in
        failwith "aborting" in
    (* --- skip row header *)
    let () = for _ = 1 to num_header_rows do
        let _ = Csv.next csv_reader in ()
    done in
    Csv.fold_left ~f ~init:(`List [])  csv_reader in
  Yojson.to_string x

let go ppf file' ~skip_validate =
  let csv_in = open_in file' in
  let csv_reader = Csv.of_channel csv_in in
  let json' = if skip_validate then
    to_json_no_validate csv_reader
  else
    to_json_validate csv_reader in
  let () =
    Fmt.pf ppf "@[<v>%s@]@." json' in
  Csv.close_in csv_reader
