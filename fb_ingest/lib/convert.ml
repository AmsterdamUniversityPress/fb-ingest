open Types

module R = Rresult.R
let (let*) = Rresult.R.bind
let ($) = Util.($)

let process_row_validate row_num = function
  (* --- @todo repeated *)
  | xs when List.length xs <> num_cols ->
    let s = Fmt.str "Wrong number of columns in row %d (wanted %d, got %d)" row_num num_cols (List.length xs) in
    Error (`Msg s)
  | xs ->
    let col_num = ref (-1) in
    let do_col' col_t =
      let () = col_num := !col_num + 1 in
      Process.do_col (row_num, !col_num) (xs $ !col_num) col_t ~skip_validate:false in
    let do_col_optional' col_t =
      let () = col_num := !col_num + 1 in
      Process.do_col_optional (row_num, !col_num) (xs $ !col_num) col_t ~skip_validate:false in

    let* id = do_col' col_id in
    let* naam_organisatie = do_col' col_naam_organisatie in
    let* categorie = do_col' col_categorie in
    let* website = do_col' col_website in
    let* type_organisatie = do_col_optional' col_type_organisatie in

    let* naam_moeder_organisatie = do_col_optional' col_naam_moeder_organisatie in
    let* oprichtings_datum = do_col' col_oprichtings_datum in
    let* rechtsvorm = do_col' col_rechtsvorm in
    let* kvk_number = do_col' col_kvk_number in
    let* anbi_status = do_col' col_anbi_status in
    let* rsin = do_col' col_rsin in
    let* directeur_algemeen_geslacht = do_col_optional' col_directeur_algemeen_geslacht in
    let* directeur_algemeen_voorletters = do_col_optional' col_directeur_algemeen_voorletters in
    let* directeur_algemeen_tussenvoegsel = do_col_optional' col_directeur_algemeen_tussenvoegsel in
    let* directeur_algemeen_achternaam = do_col_optional' col_directeur_algemeen_achternaam in
    let* bestuursvoorzitter_geslacht = do_col_optional' col_bestuursvoorzitter_geslacht in
    let* bestuursvoorzitter_voorletters = do_col_optional' col_bestuursvoorzitter_voorletters in
    let* bestuursvoorzitter_tussenvoegsel = do_col_optional' col_bestuursvoorzitter_tussenvoegsel in
    let* bestuursvoorzitter_achternaam = do_col_optional' col_bestuursvoorzitter_achternaam in
    let* bestuurssecretaris_geslacht = do_col_optional' col_bestuurssecretaris_geslacht in
    let* bestuurssecretaris_voorletters = do_col_optional' col_bestuurssecretaris_voorletters in
    let* bestuurssecretaris_tussenvoegsel = do_col_optional' col_bestuurssecretaris_tussenvoegsel in
    let* bestuurssecretaris_achternaam = do_col_optional' col_bestuurssecretaris_achternaam in
    let* bestuurspenningmeester_geslacht = do_col_optional' col_bestuurspenningmeester_geslacht in
    let* bestuurspenningmeester_voorletters = do_col_optional' col_bestuurspenningmeester_voorletters in
    let* bestuurspenningmeester_tussenvoegsel = do_col_optional' col_bestuurspenningmeester_tussenvoegsel in
    let* bestuurspenningmeester_achternaam = do_col_optional' col_bestuurspenningmeester_achternaam in
    let* bestuurslid3_geslacht = do_col_optional' col_bestuurslid3_geslacht in
    let* bestuurslid3_voorletters = do_col_optional' col_bestuurslid3_voorletters in
    let* bestuurslid3_tussenvoegsel = do_col_optional' col_bestuurslid3_tussenvoegsel in
    let* bestuurslid3_achternaam = do_col_optional' col_bestuurslid3_achternaam in
    let* bestuurslid4_geslacht = do_col_optional' col_bestuurslid4_geslacht in
    let* bestuurslid4_voorletters = do_col_optional' col_bestuurslid4_voorletters in
    let* bestuurslid4_tussenvoegsel = do_col_optional' col_bestuurslid4_tussenvoegsel in
    let* bestuurslid4_achternaam = do_col_optional' col_bestuurslid4_achternaam in
    let* bestuurslid5_geslacht = do_col_optional' col_bestuurslid5_geslacht in
    let* bestuurslid5_voorletters = do_col_optional' col_bestuurslid5_voorletters in
    let* bestuurslid5_tussenvoegsel = do_col_optional' col_bestuurslid5_tussenvoegsel in
    let* bestuurslid5_achternaam = do_col_optional' col_bestuurslid5_achternaam in
    let* bestuurslid6_geslacht = do_col_optional' col_bestuurslid6_geslacht in
    let* bestuurslid6_voorletters = do_col_optional' col_bestuurslid6_voorletters in
    let* bestuurslid6_tussenvoegsel = do_col_optional' col_bestuurslid6_tussenvoegsel in
    let* bestuurslid6_achternaam = do_col_optional' col_bestuurslid6_achternaam in
    let* doelstelling = do_col' col_doelstelling in
    let* stichter = do_col_optional' col_stichter in
    let* historie = do_col_optional' col_historie in

    Ok (Fonds {
      id;
      naam_organisatie;
      categorie;
      website;
      type_organisatie;
      naam_moeder_organisatie;
      oprichtings_datum;
      rechtsvorm;
      kvk_number;
      anbi_status;
      rsin;
      directeur_algemeen_geslacht;
      directeur_algemeen_voorletters;
      directeur_algemeen_tussenvoegsel;
      directeur_algemeen_achternaam;
      bestuursvoorzitter_geslacht;
      bestuursvoorzitter_voorletters;
      bestuursvoorzitter_tussenvoegsel;
      bestuursvoorzitter_achternaam;
      bestuurssecretaris_geslacht;
      bestuurssecretaris_voorletters;
      bestuurssecretaris_tussenvoegsel;
      bestuurssecretaris_achternaam;
      bestuurspenningmeester_geslacht;
      bestuurspenningmeester_voorletters;
      bestuurspenningmeester_tussenvoegsel;
      bestuurspenningmeester_achternaam;
      bestuurslid3_geslacht;
      bestuurslid3_voorletters;
      bestuurslid3_tussenvoegsel;
      bestuurslid3_achternaam;
      bestuurslid4_geslacht;
      bestuurslid4_voorletters;
      bestuurslid4_tussenvoegsel;
      bestuurslid4_achternaam;
      bestuurslid5_geslacht;
      bestuurslid5_voorletters;
      bestuurslid5_tussenvoegsel;
      bestuurslid5_achternaam;
      bestuurslid6_geslacht;
      bestuurslid6_voorletters;
      bestuurslid6_tussenvoegsel;
      bestuurslid6_achternaam;
      doelstelling;
      historie;
      stichter;
    })

let process_row_no_validate row_num = function
  (* --- @todo repeated *)
  | xs when List.length xs <> num_cols ->
    let s = Fmt.str "Wrong number of columns in row %d (wanted %d, got %d)" row_num num_cols (List.length xs) in
    Error (`Msg s)
  | xs ->
    let f x = `String x in
    Ok (`List (List.map f xs))
