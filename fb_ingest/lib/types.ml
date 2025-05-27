let num_cols = 92
let (let*) = Result.bind
let (let+) = fun a b -> Result.map b a

let bool_of_string_nl = function
  | "Ja" -> Ok true
  | "ja" -> Ok true
  | "Nee" -> Ok false
  | "nee" -> Ok false
  | x -> Error (`Msg (Fmt.str "Can't convert %s to bool" x))
let validate_bool_str = {| (Ja|ja|Nee|nee) |}
let validate_int_str = {| -? \d+ |}
let validate_text_str = {| .+ |}

let _validate_url_strict_str = {| (https?://)? ((\w|-)+\.)* (\w|-)+ (/ [\w \d % \( \) \. _ -]*)* (\#[\w\d%-]*)? (\? [\w \d \( \) % & = ; \. -]*)? |}
let _validate_url_full_str =
  {| (?:(?:(?:https?|ftp):)?\/\/)(?:\S+(?::\S*)?@)?(?: |} ^
  {| (?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?: |} ^
  "[a-z0-9\u{00a1}-\u{ffff}][a-z0-9\u{00a1}-\u{ffff}_-]{0,62})?" ^
  "[a-z0-9\u{00a1}-\u{ffff}]\\.)+(?:[a-z\u{00a1}-\u{ffff}]" ^
  {| {2,}\.?))(?::\d{2,5})?(?:[/?\#]\S*)? |}

let validate_url_str = _validate_url_strict_str

let validate pattern str =
  (* (<star>UCP) is a special marker recognized by libpcre, meaning that we want to enable Unicode property matching.
   * We need this so that \w matches all Unicode letters and numbers (\w becomes the union of \p{L} and \p{N}) *)
  let pattern' = "(*UCP)" ^ pattern in
  try
    let rex = Pcre.regexp ~flags:[`UTF8; `MULTILINE; `EXTENDED] pattern' in
    let subs = Pcre.exec ~rex str in
    let m0 = Pcre.get_substring subs 0 in
    Ok m0
  with Not_found -> Error (`Msg (Fmt.str "\n  patt=%s\n  target=%s\n\n" pattern' str))

let validate_bool = validate validate_bool_str
let validate_int = validate validate_int_str
let validate_text = validate validate_text_str
let validate_url = validate validate_url_str

(* --- this sucks a bit -- deriving yojson makes `Id of string` into ["Id", "xxx"], and we just want "xxx". So we need to use this on every type which has a constructor. *)
let fix_json_variant f x =
  match f x with
  | `List xs -> List.nth xs 1
  | _ -> failwith "unexpected: fix_json_variant got a non-list"

module Url = struct
  type t = Url of string
  [@@deriving yojson]
  let map f (Url s) = Url (f s)
  let to_yojson x = fix_json_variant to_yojson x
end

let url_fix_protocol =
  let f url' =
    let no_protocol' =
      not (String.starts_with ~prefix:"http://" url') &&
      not (String.starts_with ~prefix:"https://" url') in
    if no_protocol' then "http://" ^ url'
    else url' in
  Url.map f

let mk_url x = Url.Url x

module Column = struct
  type 'a t = T of 'a
  let mk x = T x
  let get (T x) = x
  let map f (T x) = T (f x)
end

type id = Id of int
[@@deriving yojson]

let mk_id_r str =
  let+ str' = validate_int str in
  Id (int_of_string str')

let id_to_yojson x = fix_json_variant id_to_yojson x

type naam_organisatie = NaamOrganisatie of string
[@@deriving yojson]

let mk_naam_organisatie_r str =
  let+ str' = validate_text str in
  NaamOrganisatie str'

let naam_organisatie_to_yojson x = fix_json_variant naam_organisatie_to_yojson x

type categorie = Categorie of string
[@@deriving yojson]

let categorie_to_yojson x = fix_json_variant categorie_to_yojson x
let map_categorie f (Categorie x) = Categorie (f x)
let mk_categorie s = Categorie s
let mk_categories_r str =
  str
  |> validate_text
  |> Result.map (fun str ->
      str
      |> String.split_on_char (';')
      |> List.map (mk_categorie % String.trim)
    )

type website = Website of Url.t
[@@deriving yojson]

let website_to_yojson x = fix_json_variant website_to_yojson x
let mk_website_r str =
  str
  |> validate_url
  |> Result.map (fun str ->
      str
      |> mk_url
      |> url_fix_protocol
      |> fun url -> Website url
    )

type type_organisatie = TypeOrganisatie of string
[@@deriving yojson]

let type_organisatie_to_yojson x = fix_json_variant type_organisatie_to_yojson x
let mk_type_organisatie_r str =
  str
  |> validate_text
  |> Result.map (fun str -> TypeOrganisatie str)

type naam_moeder_organisatie = NaamMoederOrganisatie of string
[@@deriving yojson]

type oprichtings_datum = OprichtingsDatum of string
[@@deriving yojson]

let mk_oprichtings_datum_r str =
  str
  |> validate_text
  |> Result.map (fun str -> OprichtingsDatum str)

let oprichtings_datum_to_yojson x = fix_json_variant oprichtings_datum_to_yojson x

let naam_moeder_organisatie_to_yojson x = fix_json_variant naam_moeder_organisatie_to_yojson x
let mk_naam_moeder_organisatie_r str =
  str
  |> validate_text
  |> Result.map (fun str -> NaamMoederOrganisatie str)

type rechtsvorm = Rechtsvorm of string
[@@deriving yojson]

let rechtsvorm_to_yojson x = fix_json_variant rechtsvorm_to_yojson x
let mk_rechtsvorm_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Rechtsvorm str)

type kvk_number = KvkNumber of string
[@@deriving yojson]

let kvk_number_to_yojson x = fix_json_variant kvk_number_to_yojson x
let mk_kvk_number_r str =
  let+ str' = validate_text str in
  KvkNumber str'

type anbi_status = AnbiStatus of bool
[@@deriving yojson]

let anbi_status_to_yojson x = fix_json_variant anbi_status_to_yojson x
let mk_anbi_status_r str =
  let* str' = validate_bool str in
  str'
  |> bool_of_string_nl
  |> Result.map (fun b -> AnbiStatus b)

type rsin = Rsin of int
[@@deriving yojson]

let rsin_to_yojson x = fix_json_variant rsin_to_yojson x
let mk_rsin_r str =
  str
  |> validate_int
  |> Result.map (fun str -> Rsin (int_of_string str))

type directeur_algemeen = DirecteurAlgemeen of string
[@@deriving yojson]
let directeur_algemeen_to_yojson x = fix_json_variant directeur_algemeen_to_yojson x

let mk_directeur_algemeen_geslacht_r str =
  str
  |> validate_text

let mk_directeur_algemeen_voorletters_r str =
  str
  |> validate_text

let mk_directeur_algemeen_tussenvoegsel_r str =
  str
  |> validate_text

let mk_directeur_algemeen_achternaam_r str =
  str
  |> validate_text

let mk_directeur_algemeen_r str =
  str
  |> validate_text

let mk_person_option (kind, row_num, col_num) mk geslacht voorletters tussenvoegsel achternaam =
  let args = [geslacht; voorletters; tussenvoegsel; achternaam] in
  if Util.Option.all_none args then None else
  if Option.is_none achternaam then
    let () = Fmt.epr "Warning: achternaam is empty for %s, row=%d, col=%d, skipping row@." kind row_num col_num in
    None else
  Some (mk (Util.Option.join_some " " args))

let mk_directeur_algemeen_option row_num col_num = mk_person_option ("directeur_algemeen", row_num, col_num) (fun x -> DirecteurAlgemeen x)

type bestuursvoorzitter = Bestuursvoorzitter  of string
[@@deriving yojson]

let mk_bestuursvoorzitter_geslacht_r str = str |> validate_text
let mk_bestuursvoorzitter_voorletters_r str = str |> validate_text
let mk_bestuursvoorzitter_tussenvoegsel_r str = str |> validate_text
let mk_bestuursvoorzitter_achternaam_r str = str |> validate_text

let mk_bestuursvoorzitter_option row_num col_num = mk_person_option ("bestuursvoorzitter", row_num, col_num) (fun x -> Bestuursvoorzitter x)
let bestuursvoorzitter_to_yojson x = fix_json_variant bestuursvoorzitter_to_yojson x

type bestuurssecretaris = Bestuurssecretaris of string
[@@deriving yojson]
let bestuurssecretaris_to_yojson = fix_json_variant bestuurssecretaris_to_yojson

let mk_bestuurssecretaris_geslacht_r str = str |> validate_text
let mk_bestuurssecretaris_voorletters_r str = str |> validate_text
let mk_bestuurssecretaris_tussenvoegsel_r str = str |> validate_text
let mk_bestuurssecretaris_achternaam_r str = str |> validate_text

let mk_bestuurssecretaris_option row_num col_num = mk_person_option ("bestuurssecretaris", row_num, col_num) (fun x -> Bestuurssecretaris x)

type bestuurspenningmeester = Bestuurspenningmeester of string
[@@deriving yojson]

let mk_bestuurspenningmeester_geslacht_r str = str |> validate_text
let mk_bestuurspenningmeester_voorletters_r str = str |> validate_text
let mk_bestuurspenningmeester_tussenvoegsel_r str = str |> validate_text
let mk_bestuurspenningmeester_achternaam_r str = str |> validate_text

let mk_bestuurspenningmeester_option row_num col_num = mk_person_option ("bestuurspenningmeester", row_num, col_num) (fun x -> Bestuurspenningmeester x)

let bestuurspenningmeester_to_yojson x = fix_json_variant bestuurspenningmeester_to_yojson x

type bestuurslid = Bestuurslid of string
[@@deriving yojson]

let mk_bestuurslid_geslacht_r str = str |> validate_text
let mk_bestuurslid_voorletters_r str = str |> validate_text
let mk_bestuurslid_tussenvoegsel_r str = str |> validate_text
let mk_bestuurslid_achternaam_r str = str |> validate_text

let mk_bestuurslid_option row_num col_num = mk_person_option ("bestuurslid", row_num, col_num) (fun x -> Bestuurslid x)

let bestuurslid_to_yojson x = fix_json_variant bestuurslid_to_yojson x

type doelstelling = Doelstelling of string
[@@deriving yojson]

let mk_doelstelling_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> Doelstelling str)

let doelstelling_to_yojson x = fix_json_variant doelstelling_to_yojson x

type stichter = Stichter of string
[@@deriving yojson]

let mk_stichter_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> Stichter str)

let stichter_to_yojson x = fix_json_variant stichter_to_yojson x

type historie = Historie of string
[@@deriving yojson]

let mk_historie_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> Historie str)

let historie_to_yojson x = fix_json_variant historie_to_yojson x

type beleidsplan_op_website = BeleidsplanOpWebsite of bool
[@@deriving yojson]

let mk_beleidsplan_op_website_r str =
  let* str' = validate_bool str in
  str'
  |> bool_of_string_nl
  |> Result.map (fun b -> BeleidsplanOpWebsite b)

let beleidsplan_op_website_to_yojson x = fix_json_variant beleidsplan_op_website_to_yojson x

type doelgroep = Doelgroep of string
[@@deriving yojson]

let mk_doelgroep_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Some (Doelgroep str))

let doelgroep_to_yojson x = fix_json_variant doelgroep_to_yojson x

type doelgroep_overig = DoelgroepOverig of string
[@@deriving yojson]

let mk_doelgroep_overig_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> DoelgroepOverig str)

let doelgroep_overig_to_yojson x = fix_json_variant doelgroep_overig_to_yojson x

type activiteiten_beschrijving = ActiviteitenBeschrijving of string
[@@deriving yojson]

let mk_activiteiten_beschrijving_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> ActiviteitenBeschrijving str)

let activiteiten_beschrijving_to_yojson x = fix_json_variant activiteiten_beschrijving_to_yojson x

type interventie_niveau = InterventieNiveau of string
[@@deriving yojson]

let mk_interventie_niveau_r str =
  str
  |> validate_text
  |> Result.map (fun str -> InterventieNiveau str)

let interventie_niveau_to_yojson x = fix_json_variant interventie_niveau_to_yojson x

type werk_regio = WerkRegio of string
[@@deriving yojson]

let mk_werk_regio_r str =
  str
  |> validate_text
  |> Result.map (fun str -> WerkRegio str)

let werk_regio_to_yojson x = fix_json_variant werk_regio_to_yojson x

type landen = Landen of string
[@@deriving yojson]

let mk_landen_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Landen str)

let landen_to_yojson x = fix_json_variant landen_to_yojson x

type regio_in_nederland = RegioInNederland of string
[@@deriving yojson]
let mk_regio_in_nederland_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> RegioInNederland str)

let regio_in_nederland_to_yojson x = fix_json_variant regio_in_nederland_to_yojson x

type plaats_in_nederland = PlaatsInNederland of string
[@@deriving yojson]

let mk_plaats_in_nederland_r str =
  str
  |> validate_text
  |> Result.map (fun str -> PlaatsInNederland str)

let plaats_in_nederland_to_yojson x = fix_json_variant plaats_in_nederland_to_yojson x

(* --- regio is a combination of landen, regio_in_nederland, plaats_in_nederland *)
type regio = Regio of string
[@@deriving yojson]
let regio_to_yojson x = fix_json_variant regio_to_yojson x

let split_regio s =
  let rex = Pcre.regexp ~flags:[`UTF8] {|\s*,\s*|} in
  Pcre.split ~rex s

let mk_regios landen_opt regio_in_nederland_opt plaats_in_nederland_opt =
  let split_landen (Landen l) = split_regio l in
  let split_regio_nl (RegioInNederland r) = split_regio r in
  let split_plaats_nl (PlaatsInNederland p) = split_regio p in
  let to_list' f = function
    | None -> []
    | Some x -> f x in
  let landen = to_list' split_landen landen_opt in
  let regio_nl = to_list' split_regio_nl regio_in_nederland_opt in
  let plaats_nl = to_list' split_plaats_nl plaats_in_nederland_opt in
  [landen; regio_nl; plaats_nl]
  |> List.concat
  |> List.map (fun x -> Regio x)

type besteding_budget = BestedingBudget of string
[@@deriving yojson]

let mk_besteding_budget_r str =
  str
  |> validate_text
  |> Result.map (fun str -> BestedingBudget str)

let besteding_budget_to_yojson x = fix_json_variant besteding_budget_to_yojson x

type ondersteunde_projecten = OndersteundeProjecten of string
[@@deriving yojson]

let mk_ondersteunde_projecten_r str =
  str
  |> validate_text
  |> Result.map (fun str -> OndersteundeProjecten str)

let ondersteunde_projecten_to_yojson x = fix_json_variant ondersteunde_projecten_to_yojson x

type fin_fonds = FinFonds of string
[@@deriving yojson]

let mk_fin_fonds_r str =
  str
  |> validate_text
  |> Result.map (fun str -> FinFonds str)

let fin_fonds_to_yojson x = fix_json_variant fin_fonds_to_yojson x

type max_ondersteuning = MaxOndersteuning of string
[@@deriving yojson]

let mk_max_ondersteuning_r str =
  str
  |> validate_text
  |> Result.map (fun str -> MaxOndersteuning str)

let max_ondersteuning_to_yojson x = fix_json_variant max_ondersteuning_to_yojson x

type min_ondersteuning = MinOndersteuning of string
[@@deriving yojson]

let mk_min_ondersteuning_r str =
  str
  |> validate_text
  |> Result.map (fun str -> MinOndersteuning str)

let min_ondersteuning_to_yojson x = fix_json_variant min_ondersteuning_to_yojson x

type beschrijving_project_aanmerking = BeschrijvingProjectAanmerking of string
[@@deriving yojson]

let mk_beschrijving_project_aanmerking_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> BeschrijvingProjectAanmerking str)

let beschrijving_project_aanmerking_to_yojson x = fix_json_variant beschrijving_project_aanmerking_to_yojson x

type doorloop_tijd_act = DoorloopTijdAct of string
[@@deriving yojson]

let mk_doorloop_tijd_act_r str =
  str
  |> validate_text
  |> Result.map (fun str -> DoorloopTijdAct str)

let doorloop_tijd_act_to_yojson x = fix_json_variant doorloop_tijd_act_to_yojson x

type fonds_type_aanvraag = FondsTypeAanvraag of string
[@@deriving yojson]

let mk_fonds_type_aanvraag_r str =
  str
  |> validate_text
  |> Result.map (fun str -> FondsTypeAanvraag str)

let fonds_type_aanvraag_to_yojson x = fix_json_variant fonds_type_aanvraag_to_yojson x

type uitsluiting = Uitsluiting of string
[@@deriving yojson]

let mk_uitsluiting_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> Uitsluiting str)

let uitsluiting_to_yojson x = fix_json_variant uitsluiting_to_yojson x

type op_aanvraag = OpAanvraag of bool
[@@deriving yojson]

let mk_op_aanvraag_r str =
  let* str' = validate_bool str in
  str'
  |> bool_of_string_nl
  |> Result.map (fun b -> OpAanvraag b)

let op_aanvraag_to_yojson x = fix_json_variant op_aanvraag_to_yojson x

type doorloop_tijd = DoorloopTijd of string
[@@deriving yojson]

let mk_doorloop_tijd_r str =
  str
  |> validate_text
  |> Result.map (fun str -> DoorloopTijd str)

let doorloop_tijd_to_yojson x = fix_json_variant doorloop_tijd_to_yojson x

type aanvraag_procedure = AanvraagProcedure of string
[@@deriving yojson]

let mk_aanvraag_procedure_r str =
  str
  |> String.trim
  |> validate_text
  |> Result.map (fun str -> AanvraagProcedure str)

let aanvraag_procedure_to_yojson x = fix_json_variant aanvraag_procedure_to_yojson x

type url_aanvraag_procedure = UrlAanvraagProcedure of Url.t
[@@deriving yojson]

let mk_url_aanvraag_procedure_r str =
  str
  |> validate_text
  |> Result.map (fun str ->
      str
      |> mk_url
      |> url_fix_protocol
      |> fun url -> UrlAanvraagProcedure url
    )

let url_aanvraag_procedure_to_yojson x = fix_json_variant url_aanvraag_procedure_to_yojson x

type eigen_vermogen = EigenVermogen of string
[@@deriving yojson]

type inkomsten_eigen_vermogen = InkomstenEigenVermogen of string
[@@deriving yojson]

let mk_inkomsten_eigen_vermogen_r str =
  str
  |> validate_text
  |> Result.map (fun str -> InkomstenEigenVermogen str)

let inkomsten_eigen_vermogen_to_yojson x = fix_json_variant inkomsten_eigen_vermogen_to_yojson x

let mk_eigen_vermogen_r str =
  str
  |> validate_text
  |> Result.map (fun str -> EigenVermogen str)

let eigen_vermogen_to_yojson x = fix_json_variant eigen_vermogen_to_yojson x

type herkomst_middelen = HerkomstMiddelen of string
[@@deriving yojson]

let mk_herkomst_middelen_r str =
  str
  |> validate_text
  |> Result.map (fun str -> HerkomstMiddelen str)

let herkomst_middelen_to_yojson x = fix_json_variant herkomst_middelen_to_yojson x

type boekjaar = Boekjaar of string
[@@deriving yojson]

let mk_boekjaar_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Boekjaar str)

let boekjaar_to_yojson x = fix_json_variant boekjaar_to_yojson x

type url_jaarverslag = UrlJaarverslag of Url.t
[@@deriving yojson]

let mk_url_jaarverslag_r str =
  str
  |> validate_text
  |> Result.map (fun str ->
      str
      |> mk_url
      |> url_fix_protocol
      |> fun url -> UrlJaarverslag url
    )

let url_jaarverslag_to_yojson x = fix_json_variant url_jaarverslag_to_yojson x

type contact = Contact of string
[@@deriving yojson]

let mk_contact_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Contact str)

let contact_to_yojson x = fix_json_variant contact_to_yojson x

let mk_cpiaanvragen_geslacht_r str = str |> validate_text
let mk_cpiaanvragen_voorletters_r str = str |> validate_text
let mk_cpiaanvragen_tussenvoegsel_r str = str |> validate_text
let mk_cpiaanvragen_achternaam_r str = str |> validate_text

type cpfinaanvragen = Cpfinaanvragen of string
[@@deriving yojson]

let mk_cpfinaanvragen_option row_num col_num = mk_person_option ("cpfinaanvragen", row_num, col_num) (fun x -> Cpfinaanvragen x)

let cpfinaanvragen_to_yojson x = fix_json_variant cpfinaanvragen_to_yojson x

let mk_postadres_straat_r str = str |> validate_text
let mk_postadres_huisnummer_r str = str |> validate_text
let mk_postadres_huisnummer_ext_r str = str |> validate_text
let mk_postadres_postcode_r str = str |> validate_text
let mk_postadres_plaats_r str = str |> validate_text

type postadres = Postadres of string
[@@deriving yojson]

let postadres_to_yojson x = fix_json_variant postadres_to_yojson x

let mk_postadres_option (row_num, col_num) straat huisnummer huisnummer_ext postcode plaats =
  let warn' s = Fmt.epr "Warning: %s, row_num=%d, col_num=%d, skipping row@." s row_num col_num in
  let args = [straat; huisnummer; huisnummer_ext; postcode; plaats] in
  if Util.Option.all_none args then None
  else if Option.(is_some huisnummer_ext && is_none huisnummer) then
    let () = warn' (Fmt.str "Invalid huisnummer/huisnummer_ext combination, straat was %a" (Fmt.(option string)) straat) in
    None
  else match List.find_opt Option.is_none [straat; huisnummer; postcode; plaats] with
    | Some _ -> let () = warn' (Fmt.str "Missing postadres_xxx fields") in None
    | None ->
      (* --- @todo kan mooier *)
      let huisnummer' = Util.Option.join_some "" [huisnummer; huisnummer_ext] in
      let huisnummer'' = match huisnummer' with
        | "" -> None
        | x -> Some x in
      let args' = [straat; huisnummer''; postcode; plaats] in
      Some (Postadres (Util.Option.join_some " " args'))

type email = Email of string
[@@deriving yojson]

let mk_email_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Email str)

let email_to_yojson x = fix_json_variant email_to_yojson x

type telefoon = Telefoon of string
[@@deriving yojson]

let mk_telefoon_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Telefoon str)

let telefoon_to_yojson x = fix_json_variant telefoon_to_yojson x

type telefoon_fin_aanvragen = TelefoonFinAanvragen of string
[@@deriving yojson]

let mk_telefoon_fin_aanvragen_r str =
  str
  |> validate_text
  |> Result.map (fun str -> TelefoonFinAanvragen str)

let telefoon_fin_aanvragen_to_yojson x = fix_json_variant telefoon_fin_aanvragen_to_yojson x

type trefwoord = Trefwoord of string
[@@deriving yojson]

let mk_trefwoorden_r str =
  str
  |> validate_text
  |> Result.map (fun str ->
      str
      |> String.split_on_char ','
      |> List.map (fun s -> Trefwoord (String.trim s)))

let map_trefwoord f (Trefwoord t) = Trefwoord (f t)

let trefwoord_to_yojson x = fix_json_variant trefwoord_to_yojson x

type fonds = Fonds of {
  id: id;
  uuid: string;
  naam_organisatie: naam_organisatie;
  categories: categorie list;
  website: website option;
  type_organisatie: type_organisatie option;
  naam_moeder_organisatie: naam_moeder_organisatie option;
  oprichtings_datum: oprichtings_datum option;
  rechtsvorm: rechtsvorm;
  kvk_number: kvk_number option;
  anbi_status: anbi_status;
  rsin: rsin option;
  directeur_algemeen: directeur_algemeen option;
  bestuursvoorzitter: bestuursvoorzitter option;
  bestuurssecretaris: bestuurssecretaris option;
  bestuurspenningmeester: bestuurspenningmeester option;
  bestuursleden_overig: bestuurslid list;
  doelstelling: doelstelling;
  stichter: stichter option;
  historie: historie option;
  beleidsplan_op_website: beleidsplan_op_website;
  doelgroep: doelgroep option;
  doelgroep_overig: doelgroep_overig option;
  activiteiten_beschrijving: activiteiten_beschrijving option;
  interventie_niveau: interventie_niveau option;
  werk_regio: werk_regio option;
  regios: regio list;
  landen: landen option;
  regio_in_nederland: regio_in_nederland option;
  plaats_in_nederland: plaats_in_nederland option;
  besteding_budget: besteding_budget option;
  ondersteunde_projecten: ondersteunde_projecten option;
  fin_fonds: fin_fonds option;
  max_ondersteuning: max_ondersteuning option;
  min_ondersteuning: min_ondersteuning option;
  beschrijving_project_aanmerking: beschrijving_project_aanmerking option;
  doorloop_tijd_act: doorloop_tijd_act option;
  fonds_type_aanvraag: fonds_type_aanvraag option;
  uitsluiting: uitsluiting option;
  op_aanvraag: op_aanvraag option;
  doorloop_tijd: doorloop_tijd option;
  aanvraag_procedure: aanvraag_procedure option;
  url_aanvraag_procedure: url_aanvraag_procedure option;
  eigen_vermogen: eigen_vermogen option;
  inkomsten_eigen_vermogen: inkomsten_eigen_vermogen option;
  herkomst_middelen: herkomst_middelen option;
  boekjaar: boekjaar option;
  url_jaarverslag: url_jaarverslag option;
  contact: contact option;
  cpfinaanvragen: cpfinaanvragen option;
  postadres: postadres option;
  email: email option;
  telefoon: telefoon option;
  telefoon_fin_aanvragen: telefoon_fin_aanvragen option;
  trefwoorden: trefwoord list;
}
[@@deriving yojson]

let fonds_to_yojson x = fix_json_variant fonds_to_yojson x

type t = fonds list
[@@deriving yojson]

let to_json_string = Yojson.Safe.to_string % to_yojson
