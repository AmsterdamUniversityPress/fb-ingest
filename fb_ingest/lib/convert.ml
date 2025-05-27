open Types

let (let*) = Result.bind
let ($) = Util.($)

let id x = x

let normalize_keyword kw = kw
  (* --- note, we do lowercase normalization in elastic as well *)
  |> String.lowercase_ascii
  |> String.trim
let fix_categorie = map_categorie normalize_keyword
let fix_trefwoorden = map_trefwoord normalize_keyword
let normalize_categories = List.map fix_categorie
let normalize_trefwoorden = List.map fix_trefwoorden

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
    let do_col_optional_optional' make name =
      let () = col_num := !col_num + 1 in
      Process.do_col_optional_optional (row_num, !col_num) (xs $ !col_num) make name in

    let* theId = do_col' mk_id_r "id" in
    let uuid = "one" in
    (* let uuid = Util.mk_random_uuid () in *)
    let* naam_organisatie = do_col' mk_naam_organisatie_r "naam_organistatie" in
    let* categories = Result.map normalize_categories (do_col_non_empty_list' mk_categories_r "categories") in
    let* website = do_col_optional' mk_website_r "website" in
    let* type_organisatie = do_col_optional' mk_type_organisatie_r "organisatie" in
    let* naam_moeder_organisatie = do_col_optional' mk_naam_moeder_organisatie_r "naam_moeder_organisatie" in
    let* oprichtings_datum = do_col_optional' mk_oprichtings_datum_r "oprichtings_datum" in
    let* rechtsvorm = do_col' mk_rechtsvorm_r "rechtsvorm" in
    let* kvk_number = do_col_optional' mk_kvk_number_r "kvk_number" in
    let* anbi_status = do_col' mk_anbi_status_r "anbi_status" in
    let* rsin = do_col_optional' mk_rsin_r "rsin" in
    let* directeur_algemeen_geslacht = do_col_optional' mk_directeur_algemeen_geslacht_r "directeur_algemeen_geslacht" in
    let* directeur_algemeen_voorletters = do_col_optional' mk_directeur_algemeen_voorletters_r "directeur_algemeen_voorletters" in
    let* directeur_algemeen_tussenvoegsel = do_col_optional' mk_directeur_algemeen_tussenvoegsel_r "directeur_algemeen_tussenvoegsel" in
    let* directeur_algemeen_achternaam = do_col_optional' mk_directeur_algemeen_achternaam_r "directeur_algemeen_achternaam" in
    let directeur_algemeen = mk_directeur_algemeen_option row_num !col_num
        directeur_algemeen_geslacht
        directeur_algemeen_voorletters
        directeur_algemeen_tussenvoegsel
        directeur_algemeen_achternaam
    in
    let* bestuursvoorzitter_geslacht = do_col_optional' mk_bestuursvoorzitter_geslacht_r "bestuursvoorzitter_geslacht" in
    let* bestuursvoorzitter_voorletters = do_col_optional' mk_bestuursvoorzitter_voorletters_r "bestuursvoorzitter_voorletters" in
    let* bestuursvoorzitter_tussenvoegsel = do_col_optional' mk_bestuursvoorzitter_tussenvoegsel_r "bestuursvoorzitter_tussenvoegsel" in
    let* bestuursvoorzitter_achternaam = do_col_optional' mk_bestuursvoorzitter_achternaam_r "bestuursvoorzitter_achternaam" in
    let bestuursvoorzitter = mk_bestuursvoorzitter_option row_num !col_num
        bestuursvoorzitter_geslacht
        bestuursvoorzitter_voorletters
        bestuursvoorzitter_tussenvoegsel
        bestuursvoorzitter_achternaam in
    let* bestuurssecretaris_geslacht = do_col_optional' mk_bestuurssecretaris_geslacht_r "bestuurssecretaris_geslacht" in

    let* bestuurssecretaris_voorletters = do_col_optional' mk_bestuurssecretaris_voorletters_r "bestuurssecretaris_voorletters" in
    let* bestuurssecretaris_tussenvoegsel = do_col_optional' mk_bestuurssecretaris_tussenvoegsel_r "bestuurssecretaris_tussenvoegsel" in
    let* bestuurssecretaris_achternaam = do_col_optional' mk_bestuurssecretaris_achternaam_r "bestuurssecretaris_achternaam" in
    let bestuurssecretaris = mk_bestuurssecretaris_option row_num !col_num
      bestuurssecretaris_geslacht
      bestuurssecretaris_voorletters
      bestuurssecretaris_tussenvoegsel
      bestuurssecretaris_achternaam in
    let* bestuurspenningmeester_geslacht = do_col_optional' mk_bestuurspenningmeester_geslacht_r "bestuurspenningmeester_geslacht" in
    let* bestuurspenningmeester_voorletters = do_col_optional' mk_bestuurspenningmeester_voorletters_r "bestuurspenningmeester_voorletters" in
    let* bestuurspenningmeester_tussenvoegsel = do_col_optional' mk_bestuurspenningmeester_tussenvoegsel_r "bestuurspenningmeester_tussenvoegsel" in
    let* bestuurspenningmeester_achternaam = do_col_optional' mk_bestuursvoorzitter_achternaam_r "bestuurspenningmeester_achternaam" in
    let bestuurspenningmeester = mk_bestuurspenningmeester_option row_num !col_num
      bestuurspenningmeester_geslacht
      bestuurspenningmeester_voorletters
      bestuurspenningmeester_tussenvoegsel
      bestuurspenningmeester_achternaam in
    let* bestuurslid3_geslacht = do_col_optional' mk_bestuurslid_geslacht_r "bestuurslid3_geslacht" in
    let* bestuurslid3_voorletters = do_col_optional' mk_bestuurslid_voorletters_r "bestuurslid3_voorletters" in
    let* bestuurslid3_tussenvoegsel = do_col_optional' mk_bestuurslid_tussenvoegsel_r "bestuurslid3_tussenvoegsel" in
    let* bestuurslid3_achternaam = do_col_optional' mk_bestuurslid_achternaam_r "bestuurslid3_achternaam" in
    let bestuurslid3 = mk_bestuurslid_option row_num !col_num
      bestuurslid3_geslacht
      bestuurslid3_voorletters
      bestuurslid3_tussenvoegsel
      bestuurslid3_achternaam in
    let* bestuurslid4_geslacht = do_col_optional' mk_bestuurslid_geslacht_r "bestuurslid4_geslacht" in
    let* bestuurslid4_voorletters = do_col_optional' mk_bestuurslid_voorletters_r "bestuurslid4_voorletters" in
    let* bestuurslid4_tussenvoegsel = do_col_optional' mk_bestuurslid_tussenvoegsel_r "bestuurslid4_tussenvoegsel" in
    let* bestuurslid4_achternaam = do_col_optional' mk_bestuurslid_achternaam_r "bestuurslid4_achternaam" in
    let bestuurslid4 = mk_bestuurslid_option row_num !col_num
      bestuurslid4_geslacht
      bestuurslid4_voorletters
      bestuurslid4_tussenvoegsel
      bestuurslid4_achternaam in
    let* bestuurslid5_geslacht = do_col_optional' mk_bestuurslid_geslacht_r "bestuurslid5_geslacht" in
    let* bestuurslid5_voorletters = do_col_optional' mk_bestuurslid_voorletters_r "bestuurslid5_voorletters" in
    let* bestuurslid5_tussenvoegsel = do_col_optional' mk_bestuurslid_tussenvoegsel_r "bestuurslid5_tussenvoegsel" in
    let* bestuurslid5_achternaam = do_col_optional' mk_bestuurslid_achternaam_r "bestuurslid5_achternaam" in
    let bestuurslid5 = mk_bestuurslid_option row_num !col_num
      bestuurslid5_geslacht
      bestuurslid5_voorletters
      bestuurslid5_tussenvoegsel
      bestuurslid5_achternaam in
    let* bestuurslid6_geslacht = do_col_optional' mk_bestuurslid_geslacht_r "bestuurslid6_geslacht" in
    let* bestuurslid6_voorletters = do_col_optional' mk_bestuurslid_voorletters_r "bestuurslid6_voorletters" in
    let* bestuurslid6_tussenvoegsel = do_col_optional' mk_bestuurslid_tussenvoegsel_r "bestuurslid6_tussenvoegsel" in
    let* bestuurslid6_achternaam = do_col_optional' mk_bestuurslid_achternaam_r "bestuurslid6_achternaam" in
    let bestuurslid6 = mk_bestuurslid_option row_num !col_num
      bestuurslid6_geslacht
      bestuurslid6_voorletters
      bestuurslid6_tussenvoegsel
      bestuurslid6_achternaam in
    let bestuursleden_overig =
      [ bestuurslid3; bestuurslid4; bestuurslid5; bestuurslid6; ]
      |> List.filter_map id in
    let* doelstelling = do_col' mk_doelstelling_r "doelstelling" in
    let* stichter = do_col_optional' mk_stichter_r "stichter" in
    let* historie = do_col_optional' mk_historie_r "historie" in
    let* beleidsplan_op_website = do_col' mk_beleidsplan_op_website_r "beleidsplan_op_website" in
    let* doelgroep = do_col_optional_optional' mk_doelgroep_r "doelgroep" in
    let* doelgroep_overig = do_col_optional' mk_doelgroep_overig_r "doelgroep_overig" in
    let* activiteiten_beschrijving = do_col_optional' mk_activiteiten_beschrijving_r "activiteiten_beschrijving" in
    let* interventie_niveau = do_col_optional' mk_interventie_niveau_r "interventie_niveau" in
    let* werk_regio = do_col_optional' mk_werk_regio_r "werk_regio" in

    let* landen = do_col_optional' mk_landen_r "landen" in
    let* regio_in_nederland = do_col_optional' mk_regio_in_nederland_r "regio_in_nederland" in
    let* plaats_in_nederland = do_col_optional' mk_plaats_in_nederland_r "plaats_in_nederland" in
    let regios = mk_regios landen regio_in_nederland plaats_in_nederland in

    let* besteding_budget = do_col_optional' mk_besteding_budget_r "besteding_budget" in
    let* ondersteunde_projecten = do_col_optional' mk_ondersteunde_projecten_r "ondersteunde_projecten" in
    let* fin_fonds = do_col_optional' mk_fin_fonds_r "fin_fonds_r" in
    let* max_ondersteuning = do_col_optional' mk_max_ondersteuning_r "max_ondersteuning" in
    let* min_ondersteuning = do_col_optional' mk_min_ondersteuning_r "min_ondersteuning" in
    let* beschrijving_project_aanmerking = do_col_optional' mk_beschrijving_project_aanmerking_r "beschrijving_project_aanmerking" in
    let* doorloop_tijd_act = do_col_optional' mk_doorloop_tijd_act_r "doorloop_tijd_act" in
    let* fonds_type_aanvraag = do_col_optional' mk_fonds_type_aanvraag_r "fonds_type_aanvraag" in
    let* uitsluiting = do_col_optional' mk_uitsluiting_r "uitsluiting" in
    let* op_aanvraag = do_col_optional' mk_op_aanvraag_r "op_aanvraag" in
    let* doorloop_tijd = do_col_optional' mk_doorloop_tijd_r "doorloop_tijd" in
    let* aanvraag_procedure = do_col_optional' mk_aanvraag_procedure_r "aanvraag_procedure" in
    let* url_aanvraag_procedure = do_col_optional' mk_url_aanvraag_procedure_r "url_aanvraag_procedure" in
    let* eigen_vermogen = do_col_optional' mk_eigen_vermogen_r "eigen_vermogen" in
    let* inkomsten_eigen_vermogen = do_col_optional' mk_inkomsten_eigen_vermogen_r "inkomsten_eigen_vermogen" in
    let* herkomst_middelen = do_col_optional' mk_herkomst_middelen_r "herkomst_middelen" in
    let* boekjaar = do_col_optional' mk_boekjaar_r "boekjaar" in
    let* url_jaarverslag = do_col_optional' mk_url_jaarverslag_r "url_jaarverslag" in
    let* contact = do_col_optional' mk_contact_r "contact" in
    let* cpfinaanvragen_geslacht = do_col_optional' mk_cpiaanvragen_geslacht_r "cpfinaanvragen_geslacht" in
    let* cpfinaanvragen_voorletters = do_col_optional' mk_cpiaanvragen_voorletters_r "cpfinaanvragen_voorletters" in
    let* cpfinaanvragen_tussenvoegsel = do_col_optional' mk_cpiaanvragen_tussenvoegsel_r "cpfinaanvragen_tussenvoegsel" in
    let* cpfinaanvragen_achternaam = do_col_optional' mk_cpiaanvragen_achternaam_r "cpfinaanvragen_achternaam" in
    let cpfinaanvragen = mk_cpfinaanvragen_option row_num !col_num
      cpfinaanvragen_geslacht
      cpfinaanvragen_voorletters
      cpfinaanvragen_tussenvoegsel
      cpfinaanvragen_achternaam in
    let* postadres_straat = do_col_optional' mk_postadres_straat_r "postadres_straat" in
    let* postadres_huisnummer = do_col_optional' mk_postadres_huisnummer_r "postadres_huisnummer" in
    let* postadres_huisnummer_ext = do_col_optional' mk_postadres_huisnummer_ext_r "postadres_huisnummer_ext" in
    let* postadres_postcode = do_col_optional' mk_postadres_postcode_r "postadres_postcode" in
    let* postadres_plaats = do_col_optional' mk_postadres_plaats_r "postadres_plaats" in
    let postadres = mk_postadres_option
      (row_num, !col_num)
      postadres_straat
      postadres_huisnummer
      postadres_huisnummer_ext
      postadres_postcode
      postadres_plaats in
    let* email = do_col_optional' mk_email_r "email" in
    let* telefoon = do_col_optional' mk_telefoon_r "telefoon" in
    let* telefoon_fin_aanvragen = do_col_optional' mk_telefoon_fin_aanvragen_r "telefoon_fin_aanvragen" in
    let* trefwoorden = Rresult.R.map normalize_trefwoorden (do_col' mk_trefwoorden_r "trefwoorden") in

    Ok (Fonds {
        id = theId;
        uuid;
        naam_organisatie;
        categories;
        website;
        type_organisatie;
        naam_moeder_organisatie;
        oprichtings_datum;
        rechtsvorm;
        kvk_number;
        anbi_status;
        rsin;
        directeur_algemeen;
        bestuursvoorzitter;
        bestuurssecretaris;
        bestuurspenningmeester;
        bestuursleden_overig;
        doelstelling;
        historie;
        stichter;
        beleidsplan_op_website;
        doelgroep;
        doelgroep_overig;
        activiteiten_beschrijving;
        interventie_niveau;
        werk_regio;
        landen;
        regio_in_nederland;
        plaats_in_nederland;
        regios;
        besteding_budget;
        ondersteunde_projecten;
        fin_fonds;
        max_ondersteuning;
        min_ondersteuning;
        beschrijving_project_aanmerking;
        doorloop_tijd_act;
        fonds_type_aanvraag;
        uitsluiting;
        op_aanvraag;
        doorloop_tijd;
        aanvraag_procedure;
        url_aanvraag_procedure;
        eigen_vermogen;
        inkomsten_eigen_vermogen;
        herkomst_middelen;
        boekjaar;
        url_jaarverslag;
        contact;
        cpfinaanvragen;
        postadres;
        email;
        telefoon;
        telefoon_fin_aanvragen;
        trefwoorden;
      })

let process_row_no_validate row_num = function
  (* --- @todo repeated *)
  | xs when List.length xs <> num_cols ->
    let s = Fmt.str "Wrong number of columns in row %d (wanted %d, got %d)" row_num num_cols (List.length xs) in
    Error (`Msg s)
  | xs ->
    let f x = `String x in
    Ok (`List (List.map f xs))
