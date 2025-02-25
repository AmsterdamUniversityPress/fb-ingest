let reword_error_msg f = function
  | Ok x -> Ok x
  | Error (`Msg m) -> Error (`Msg (f m))

let validate_and_mk pattern s mk =
  (* --- `re` doesn't seem to have the X flag, so we strip spaces (but not comments) manually *)
  (* (<star>UCP) is a special marker recognized by libpcre, meaning that we want to enable Unicode property matching.
   * We need this so that \w matches all Unicode letters and numbers (\w becomes the union of \p{L} and \p{N}) *)
  let pattern' = "(*UCP)" ^ Util.String.strip_spaces pattern in
  try
    let rex = Pcre.regexp ~flags:[`UTF8; `MULTILINE] pattern' in
    let subs = Pcre.exec ~rex s in
    let m0 = Pcre.get_substring subs 0 in
    Ok begin match mk with
    | `Bool f -> f (Types.bool_of_string_nl m0)
    | `Int f -> f (int_of_string m0)
    | `Text f -> f m0
    (* | `Text' f -> f m_all *)
    | `Url f -> f (Types.mk_url m0)
    end
  with Not_found -> Error (`Msg (Fmt.str "\n  patt=%s\n  target=%s\n\n" pattern' s))

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
