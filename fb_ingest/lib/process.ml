let reword_error_msg f = function
  | Ok x -> Ok x
  | Error (`Msg m) -> Error (`Msg (f m))

let validate_and_mk pattern s mk =
  (* --- `re` doesn't seem to have the X flag, so we strip spaces (but not comments) manually *)
  let pattern' = Util.String.strip_spaces pattern in
  let re' = Re.Perl.compile_pat ~opts:[`Multiline] pattern' in
  match Re.exec_opt re' s with
  | None -> Error (`Msg (Fmt.str "\n  patt = %s\n  target = %s\n\n" pattern s))
  | Some m ->
    let m0 = Re.Group.get m 0 in
    (* let m_all = Re.Group.all m in *)
    Ok begin match mk with
    | `Bool f -> f (Types.bool_of_string_nl m0)
    | `Int f -> f (int_of_string m0)
    | `Text f -> f m0
    (* | `Text' f -> f m_all *)
    | `Url f -> f (Types.mk_url m0)
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
    (Util.String.trim s)
    (Types.Column.name row_t)
    validate'
    (Types.Column.mk row_t)

let do_col_optional (row_num, col_num) s row_t ~skip_validate =
  if s = ""
  then Ok None
  else match do_col (row_num, col_num) s row_t ~skip_validate with
    | Error e -> Error e
    | Ok r -> Ok (Some r)

let do_col_optional_optional (row_num, col_num) s row_t ~skip_validate =
  if s = ""
  then Ok None
  else do_col (row_num, col_num) s row_t ~skip_validate
