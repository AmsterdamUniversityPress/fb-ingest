let do_col (row_num, col_num) value make name =
  let reword msg =
    Fmt.str "Failed at row=%d, col=%d (%s): %s" row_num col_num name msg in
  value
  |> make
  |> Result.map_error (fun (`Msg m) -> `Msg (reword m))

let do_col_optional (row_num, col_num) value make name =
  if value = ""
  then Ok None
  else
    let ret = do_col (row_num, col_num) value make name in
    Result.map (fun r -> Some r) ret

let do_col_list (row_num, col_num) value make name =
  if value = ""
  then Ok []
  else do_col (row_num, col_num) value make name

let do_col_optional_optional (row_num, col_num) value make name =
  if value = ""
  then Ok None
  else do_col (row_num, col_num) value make name
