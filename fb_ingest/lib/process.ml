let reword_error_msg f = function
  | Ok x -> Ok x
  | Error (`Msg m) -> Error (`Msg (f m))

let validate_and_mk pattern s mk =
  (* --- `re` doesn't seem to have the X flag, so we strip spaces (but not comments) manually *)
  let pattern' = Util.strip_spaces pattern in
  let re' = Re.Perl.compile_pat pattern' in
  match Re.exec_opt re' s with
  | None -> Error (`Msg (Fmt.str "\n  patt = %s\n  target = %s\n\n" pattern s))
  | Some m -> Ok begin match mk with
    | `Default f -> f (Re.Group.get m 0)
  end

let process (row_num, col_num) s col_name pattern mk =
  let msg' msg =
    Fmt.str "Failed at row=%d, col=%d (%s): %s" row_num col_num col_name msg in
  validate_and_mk pattern s mk
  |> reword_error_msg msg'

let do_col (row_num, col_num) s row_t ~skip_validate =
  let validate' = if skip_validate then ".*" else
    ("^" ^ Types.Column.validate_pattern row_t ^ "$") in
  process
    (row_num, col_num)
    s
    (Types.Column.name row_t)
    validate'
    (Types.Column.mk row_t)
