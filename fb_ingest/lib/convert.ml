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
    let do_col_non_empty_list' col_t =
      let () = col_num := !col_num + 1 in
      let* res = Process.do_col (row_num, !col_num) (xs $ !col_num) col_t ~skip_validate:false in
      match res with
        | [] -> Error (`Msg (Fmt.str "Got empty list (row_num=%d col_num=%d)" row_num !col_num))
        | _ -> Ok res in
    let do_col_optional' col_t =
      let () = col_num := !col_num + 1 in
      Process.do_col_optional (row_num, !col_num) (xs $ !col_num) col_t ~skip_validate:false in
    let do_col_optional_optional' col_t =
      let () = col_num := !col_num + 1 in
      Process.do_col_optional_optional (row_num, !col_num) (xs $ !col_num) col_t ~skip_validate:false in

    let* _id = do_col' col_id in
    let uuid = Util.mk_random_uuid () in
    let* naam_organisatie = do_col' col_naam_organisatie in
    let* categories = do_col_non_empty_list' col_categories in
    let* website = do_col_optional' col_website in
    let* type_organisatie = do_col_optional' col_type_organisatie in
    let* naam_moeder_organisatie = do_col_optional' col_naam_moeder_organisatie in
    let* oprichtings_datum = do_col_optional' col_oprichtings_datum in
    let* rechtsvorm = do_col' col_rechtsvorm in
    let* kvk_number = do_col_optional' col_kvk_number in
    let* anbi_status = do_col' col_anbi_status in
    let* rsin = do_col_optional' col_rsin in
    let* directeur_algemeen_geslacht = do_col_optional' col_directeur_algemeen_geslacht in
    let* directeur_algemeen_voorletters = do_col_optional' col_directeur_algemeen_voorletters in
    let* directeur_algemeen_tussenvoegsel = do_col_optional' col_directeur_algemeen_tussenvoegsel in
    let* directeur_algemeen_achternaam = do_col_optional' col_directeur_algemeen_achternaam in
    let directeur_algemeen = mk_directeur_algemeen_option
        directeur_algemeen_geslacht
        directeur_algemeen_voorletters
        directeur_algemeen_tussenvoegsel
        directeur_algemeen_achternaam
    in
    let* bestuursvoorzitter_geslacht = do_col_optional' col_bestuursvoorzitter_geslacht in
    let* bestuursvoorzitter_voorletters = do_col_optional' col_bestuursvoorzitter_voorletters in
    let* bestuursvoorzitter_tussenvoegsel = do_col_optional' col_bestuursvoorzitter_tussenvoegsel in
    let* bestuursvoorzitter_achternaam = do_col_optional' col_bestuursvoorzitter_achternaam in
    let bestuursvoorzitter = mk_bestuursvoorzitter_option
        bestuursvoorzitter_geslacht
        bestuursvoorzitter_voorletters
        bestuursvoorzitter_tussenvoegsel
        bestuursvoorzitter_achternaam in
    let* bestuurssecretaris_geslacht = do_col_optional' col_bestuurssecretaris_geslacht in
    let* bestuurssecretaris_voorletters = do_col_optional' col_bestuurssecretaris_voorletters in
    let* bestuurssecretaris_tussenvoegsel = do_col_optional' col_bestuurssecretaris_tussenvoegsel in
    let* bestuurssecretaris_achternaam = do_col_optional' col_bestuurssecretaris_achternaam in
    let bestuurssecretaris = mk_bestuurssecretaris_option
      bestuurssecretaris_geslacht
      bestuurssecretaris_voorletters
      bestuurssecretaris_tussenvoegsel
      bestuurssecretaris_achternaam in
    let* bestuurspenningmeester_geslacht = do_col_optional' col_bestuurspenningmeester_geslacht in
    let* bestuurspenningmeester_voorletters = do_col_optional' col_bestuurspenningmeester_voorletters in
    let* bestuurspenningmeester_tussenvoegsel = do_col_optional' col_bestuurspenningmeester_tussenvoegsel in
    let* bestuurspenningmeester_achternaam = do_col_optional' col_bestuurspenningmeester_achternaam in
    let bestuurspenningmeester = mk_bestuurspenningmeester_option
      bestuurspenningmeester_geslacht
      bestuurspenningmeester_voorletters
      bestuurspenningmeester_tussenvoegsel
      bestuurspenningmeester_achternaam in
    let* bestuurslid3_geslacht = do_col_optional' col_bestuurslid_geslacht in
    let* bestuurslid3_voorletters = do_col_optional' col_bestuurslid_voorletters in
    let* bestuurslid3_tussenvoegsel = do_col_optional' col_bestuurslid_tussenvoegsel in
    let* bestuurslid3_achternaam = do_col_optional' col_bestuurslid_achternaam in
    let bestuurslid3 = mk_bestuurslid_option
      bestuurslid3_geslacht
      bestuurslid3_voorletters
      bestuurslid3_tussenvoegsel
      bestuurslid3_achternaam in
    let* bestuurslid4_geslacht = do_col_optional' col_bestuurslid_geslacht in
    let* bestuurslid4_voorletters = do_col_optional' col_bestuurslid_voorletters in
    let* bestuurslid4_tussenvoegsel = do_col_optional' col_bestuurslid_tussenvoegsel in
    let* bestuurslid4_achternaam = do_col_optional' col_bestuurslid_achternaam in
    let bestuurslid4 = mk_bestuurslid_option
      bestuurslid4_geslacht
      bestuurslid4_voorletters
      bestuurslid4_tussenvoegsel
      bestuurslid4_achternaam in
    let* bestuurslid5_geslacht = do_col_optional' col_bestuurslid_geslacht in
    let* bestuurslid5_voorletters = do_col_optional' col_bestuurslid_voorletters in
    let* bestuurslid5_tussenvoegsel = do_col_optional' col_bestuurslid_tussenvoegsel in
    let* bestuurslid5_achternaam = do_col_optional' col_bestuurslid_achternaam in
    let bestuurslid5 = mk_bestuurslid_option
      bestuurslid5_geslacht
      bestuurslid5_voorletters
      bestuurslid5_tussenvoegsel
      bestuurslid5_achternaam in
    let* bestuurslid6_geslacht = do_col_optional' col_bestuurslid_geslacht in
    let* bestuurslid6_voorletters = do_col_optional' col_bestuurslid_voorletters in
    let* bestuurslid6_tussenvoegsel = do_col_optional' col_bestuurslid_tussenvoegsel in
    let* bestuurslid6_achternaam = do_col_optional' col_bestuurslid_achternaam in
    let bestuurslid6 = mk_bestuurslid_option
      bestuurslid6_geslacht
      bestuurslid6_voorletters
      bestuurslid6_tussenvoegsel
      bestuurslid6_achternaam in
    let bestuursleden =
      [ bestuurslid3; bestuurslid4; bestuurslid5; bestuurslid6; ]
      |> List.filter_map id in
    let* doelstelling = do_col' col_doelstelling in
    let* stichter = do_col_optional' col_stichter in
    let* historie = do_col_optional' col_historie in
    let* beleidsplan_op_website = do_col' col_beleidsplan_op_website in
    let* doelgroep = do_col_optional_optional' col_doelgroep in
    let* doelgroep_overig = do_col_optional' col_doelgroep_overig in
    let* activiteiten_beschrijving = do_col_optional' col_activiteiten_beschrijving in
    let* interventie_niveau = do_col_optional' col_interventie_niveau in
    let* werk_regio = do_col_optional' col_werk_regio in
    let* landen = do_col_optional' col_landen in
    let* regio_in_nederland = do_col_optional' col_regio_in_nederland in
    let* plaats_in_nederland = do_col_optional' col_plaats_in_nederland in
    let* besteding_budget = do_col_optional' col_besteding_budget in
    let* ondersteunde_projecten = do_col_optional' col_ondersteunde_projecten in
    let* fin_fonds = do_col_optional' col_fin_fonds in
    let* max_ondersteuning = do_col_optional' col_max_ondersteuning in
    let* min_ondersteuning = do_col_optional' col_min_ondersteuning in
    let* beschrijving_project_aanmerking = do_col_optional' col_beschrijving_project_aanmerking in
    let* doorloop_tijd_act = do_col_optional' col_doorloop_tijd_act in
    let* fonds_type_aanvraag = do_col_optional' col_fonds_type_aanvraag in
    let* uitsluiting = do_col_optional' col_uitsluiting in
    let* op_aanvraag = do_col_optional' col_op_aanvraag in
    let* doorloop_tijd = do_col_optional' col_doorloop_tijd in
    let* aanvraag_procedure = do_col_optional' col_aanvraag_procedure in
    let* url_aanvraag_procedure = do_col_optional' col_url_aanvraag_procedure in
    let* eigen_vermogen = do_col_optional' col_eigen_vermogen in
    let* inkomsten_eigen_vermogen = do_col_optional' col_inkomsten_eigen_vermogen in
    let* herkomst_middelen = do_col_optional' col_herkomst_middelen in
    let* boekjaar = do_col_optional' col_boekjaar in
    let* url_jaarverslag = do_col_optional' col_url_jaarverslag in
    let* contact = do_col_optional' col_contact in
    let* cpfinaanvragen_geslacht = do_col_optional' col_cpfinaanvragen_geslacht in
    let* cpfinaanvragen_voorletters = do_col_optional' col_cpfinaanvragen_voorletters in
    let* cpfinaanvragen_tussenvoegsel = do_col_optional' col_cpfinaanvragen_tussenvoegsel in
    let* cpfinaanvragen_achternaam = do_col_optional' col_cpfinaanvragen_achternaam in
    let cpfinaanvragen = mk_cpfinaanvragen_option
      cpfinaanvragen_geslacht
      cpfinaanvragen_voorletters
      cpfinaanvragen_tussenvoegsel
      cpfinaanvragen_achternaam in
    let* postadres_straat = do_col_optional' col_postadres_straat in
    let* postadres_huisnummer = do_col_optional' col_postadres_huisnummer in
    let* postadres_huisnummer_ext = do_col_optional' col_postadres_huisnummer_ext in
    let* postadres_postcode = do_col_optional' col_postadres_postcode in
    let* postadres_plaats = do_col_optional' col_postadres_plaats in
    let postadres = mk_postadres_option
      postadres_straat
      postadres_huisnummer
      postadres_huisnummer_ext
      postadres_postcode
      postadres_plaats in
    let* email = do_col_optional' col_email in
    let* telefoon = do_col_optional' col_telefoon in
    let* telefoon_fin_aanvragen = do_col_optional' col_telefoon_fin_aanvragen in
    let* trefwoorden = do_col' col_trefwoorden in
    Ok (Fonds {
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
        bestuursleden;
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
