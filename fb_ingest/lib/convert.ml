open Types

module R = Rresult.R
let (let*) = Rresult.R.bind
let ($) = Util.($)

let process_row_validate row_num = function
  (* --- @todo repeated *)
  | xs when List.length xs <> num_cols ->
    let s = Fmt.str "Wrong number of columns in row %d" row_num in
    Error (`Msg s)
  | xs ->
    let col_num = ref (-1) in
    let do_col' col_t =
      let () = col_num := !col_num + 1 in
      Process.do_col (row_num, !col_num) (xs $ !col_num) col_t ~skip_validate:false in

    let* id = do_col' col_id in
    let* naam_organisatie = do_col' col_naam_organisatie in
    let* categorie = do_col' col_categorie in
    let* website = do_col' col_website in

    Ok (Fonds {
      id: id;
      naam_organisatie: naam_organisatie;
      categorie: categorie;
      website: website;
    })

let process_row_no_validate row_num = function
  (* --- @todo repeated *)
  | xs when List.length xs <> num_cols ->
    let s = Fmt.str "Wrong number of columns in row %d" row_num in
    Error (`Msg s)
  | xs ->
    let f x = `String x in
    Ok (`List (List.map f xs))
