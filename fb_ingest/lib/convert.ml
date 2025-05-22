open Types

let (let*) = Result.bind
let ($) = Util.($)

let id x = x

let normalize_keyword kw = kw
  (* --- note, we do lowercase normalization in elastic as well *)
  |> String.lowercase_ascii
  |> String.trim
let fix_categorie = map_categorie normalize_keyword
(* let fix_trefwoorden = map_trefwoord normalize_keyword *)
let normalize_categories = List.map fix_categorie
(* let normalize_trefwoorden = List.map fix_trefwoorden *)

let process_row_validate row_num = function
  (* --- @todo repeated *)
  | xs when List.length xs <> num_cols ->
    let s = Fmt.str "Wrong number of columns in row %d (wanted %d, got %d)" row_num num_cols (List.length xs) in
    Error (`Msg s)
  | xs ->
    let col_num = ref (-1) in
    let do_col' make name =
      let () = col_num := !col_num + 1 in
      Process.do_col (row_num, !col_num) (xs $ !col_num) make name in
    let do_col_non_empty_list' make name =
      let () = col_num := !col_num + 1 in
      let* res = Process.do_col (row_num, !col_num) (xs $ !col_num) make name in
      match res with
      | [] -> Error (`Msg (Fmt.str "Got empty list (row_num=%d col_num=%d)" row_num !col_num))
      | _ -> Ok res in
    let do_col_optional' make name =
      let () = col_num := !col_num + 1 in
      Process.do_col_optional (row_num, !col_num) (xs $ !col_num) make name in
    (* let do_col_optional_optional' make name = *)
    (* let () = col_num := !col_num + 1 in *)
    (* Process.do_col_optional_optional (row_num, !col_num) (xs $ !col_num) make name in *)

    let* theId = do_col' mk_id_r "id" in
    let uuid = Util.mk_random_uuid () in
    let* naam_organisatie = do_col' mk_naam_organisatie_r "naam_organistatie" in
    let* categories = Result.map normalize_categories (do_col_non_empty_list' mk_categories_r "categories") in
    let* website = do_col_optional' mk_website_r "website" in
    let* tweede_website = do_col_optional' mk_website_r "website" in
    let* type_organisatie = do_col_optional' mk_type_organisatie_r "organisatie" in
    let* naam_moeder_organisatie = do_col_optional' mk_naam_moeder_organisatie_r "naam_moeder_organisatie" in
    let* rechtsvorm = do_col' mk_rechtsvorm_r "rechtsvorm" in
    let* kvk_number = do_col_optional' mk_kvk_number_r "kvk_number" in
    let* anbi_status = do_col' mk_anbi_status_r "anbi_status" in
    let* rsin = do_col_optional' mk_rsin_r "rsin" in
    let* directeur = do_col_optional' mk_directeur_r "directeur" in
    let* bestuursvoorzitter = do_col_optional' mk_bestuursvoorzitter_r "bestuursvoorzitter" in
    let* bestuurssecretaris = do_col_optional' mk_bestuurssecretaris_r "bestuurssecretaris" in
    let* penningmeester = do_col_optional' mk_penningmeester_r "penningmeester" in
    let* doelstelling = do_col_optional' mk_doelstelling_r "doelstelling" in
    let* doelgroep = do_col_optional' mk_doelgroep_r "doelgroep" in
    let* activititeiten = do_col_optional' mk_activititeiten_r "activititeiten" in
    let* werkterrein_geografisch = do_col_optional' mk_werkterrein_geografisch_r "werkterrein_geografisch" in
    let* contact = do_col_optional' mk_contact_r "contact" in
    let* aanvraagprocedure = do_col_optional' mk_aanvraagprocedure_r "contact" in
    let* postadres_straat = do_col_optional' mk_postadres_straat_r "postadres_straat" in
    let* postadres_huisnummer = do_col_optional' mk_postadres_huisnummer_r "postadres_huisnummer" in
    let* postadres_huisnummer_ext = do_col_optional' mk_postadres_huisnummer_ext_r "postadres_huisnummer_ext" in
    let* postadres_postcode = do_col_optional' mk_postadres_postcode_r "postadres_postcode" in
    let* postadres_plaats = do_col_optional' mk_postadres_plaats_r "postadres_plaats" in
    let* email = do_col_optional' mk_email_r "email" in
    let* telefoon = do_col_optional' mk_telefoon_r "telefoon" in
    let* trefwoorden = do_col_non_empty_list' mk_trefwoorden_r "trefwoorden" in
    let* facebook = do_col_optional' mk_facebook_r "facebook" in
    let* linkedin = do_col_optional' mk_linkedin_r "linkedin" in
    let* instagram = do_col_optional' mk_instagram_r "instagram" in
    let* twitter = do_col_optional' mk_twitter_r "twitter" in
    let* bijzonderheden = do_col_optional' mk_bijzonderheden_r "bijzonderheden" in
    let* bijzonderheden2 = do_col_optional' mk_bijzonderheden_r "bijzonderheden2" in
    let* bijzonderheden3 = do_col_optional' mk_bijzonderheden_r "bijzonderheden3" in
    let* opmerkingen = do_col_optional' mk_opmerkingen_r "opmerkingen" in
    Ok (Fonds {
        id = theId;
        uuid;
        naam_organisatie;
        categories;
        website;
        tweede_website;
        type_organisatie;
        naam_moeder_organisatie;
        rechtsvorm;
        kvk_number;
        anbi_status;
        rsin;
        directeur;
        bestuursvoorzitter;
        bestuurssecretaris;
        penningmeester;
        doelstelling;
        doelgroep;
        activititeiten;
        werkterrein_geografisch;
        contact;
        aanvraagprocedure;
        postadres_straat;
        postadres_huisnummer;
        postadres_huisnummer_ext;
        postadres_postcode;
        postadres_plaats;
        email;
        telefoon;
        trefwoorden;
        facebook;
        linkedin;
        instagram;
        twitter;
        bijzonderheden;
        bijzonderheden2;
        bijzonderheden3;
        opmerkingen;
      })

let process_row_no_validate row_num = function
  (* --- @todo repeated *)
  | xs when List.length xs <> num_cols ->
    let s = Fmt.str "Wrong number of columns in row %d (wanted %d, got %d)" row_num num_cols (List.length xs) in
    Error (`Msg s)
  | xs ->
    let f x = `String x in
    Ok (`List (List.map f xs))
