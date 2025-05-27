let do_col (row_num, col_num) value make name =
  let reword msg =
    Fmt.str "Failed at row=%d, col=%d (%s): %s" row_num col_num name msg in
  value
  |> make
  |> Result.map_error (fun (`Msg m) -> `Msg (reword m))

let do_col_optional (row_num, col_num) value make name=
    if value = ""
    then Ok None
    else match do_col (row_num, col_num) value make name with
      | Error e -> Error e
      | Ok r -> Ok (Some r)

let do_col_optional_optional (row_num, col_num) value make name =
  if value = ""
  then Ok None
  else do_col (row_num, col_num) value make name
let do_col_optional_optional (row_num, col_num) value make name =
  if value = ""
  then Ok None
  else do_col (row_num, col_num) value make name

