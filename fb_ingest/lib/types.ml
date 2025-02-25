let num_cols = 92

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

type id = Id of int
[@@deriving yojson]
type naam_organisatie = NaamOrganisatie of string
[@@deriving yojson]
type categorie = Categorie of string
[@@deriving yojson]
let map_categorie f (Categorie x) = Categorie (f x)
type website = Website of Url.t
[@@deriving yojson]
type type_organisatie = TypeOrganisatie of string
[@@deriving yojson]
type naam_moeder_organisatie = NaamMoederOrganisatie of string
[@@deriving yojson]
type oprichtings_datum = OprichtingsDatum of string
[@@deriving yojson]
let mk_oprichtings_datum x = OprichtingsDatum x
let oprichtings_datum_to_yojson x = fix_json_variant oprichtings_datum_to_yojson x

type rechtsvorm = Rechtsvorm of string
[@@deriving yojson]
let mk_rechtsvorm x = Rechtsvorm x
let rechtsvorm_to_yojson x = fix_json_variant rechtsvorm_to_yojson x

type kvk_number = KvkNumber of string
[@@deriving yojson]
let mk_kvk_number x = KvkNumber x
let kvk_number_to_yojson x = fix_json_variant kvk_number_to_yojson x

type anbi_status = AnbiStatus of bool
[@@deriving yojson]
let mk_anbi_status x = AnbiStatus x
let anbi_status_to_yojson x = fix_json_variant anbi_status_to_yojson x

type rsin = Rsin of int
[@@deriving yojson]
let mk_rsin x = Rsin x
let rsin_to_yojson x = fix_json_variant rsin_to_yojson x

type directeur_algemeen = DirecteurAlgemeen of string
[@@deriving yojson]
let directeur_algemeen_to_yojson x = fix_json_variant directeur_algemeen_to_yojson x

let mk_person_option (kind, row_num, col_num) mk geslacht voorletters tussenvoegsel achternaam =
  let args = [geslacht; voorletters; tussenvoegsel; achternaam] in
  if Util.Option.all_none args then None else
  if Option.is_none achternaam then
    let () = Fmt.epr "Warning: achternaam is empty for %s, row=%d, col=%d, skipping row@." kind row_num col_num in
    None else
  Some (mk (Util.Option.join_some " " args))

let mk_directeur_algemeen_option row_num col_num = mk_person_option ("directeur_algemeen", row_num, col_num) (fun x -> DirecteurAlgemeen x)

type bestuursvoorzitter = Bestuursvoorzitter of string
[@@deriving yojson]
let mk_bestuursvoorzitter_option row_num col_num = mk_person_option ("bestuursvoorzitter", row_num, col_num) (fun x -> Bestuursvoorzitter x)
let bestuursvoorzitter_to_yojson x = fix_json_variant bestuursvoorzitter_to_yojson x

type bestuurssecretaris = Bestuurssecretaris of string
[@@deriving yojson]
let mk_bestuurssecretaris_option row_num col_num = mk_person_option ("bestuurssecretaris", row_num, col_num) (fun x -> Bestuurssecretaris x)
let bestuurssecretaris_to_yojson x = fix_json_variant bestuurssecretaris_to_yojson x

type bestuurspenningmeester = Bestuurspenningmeester of string
[@@deriving yojson]
let mk_bestuurspenningmeester_option row_num col_num = mk_person_option ("bestuurspenningmeester", row_num, col_num) (fun x -> Bestuurspenningmeester x)
let bestuurspenningmeester_to_yojson x = fix_json_variant bestuurspenningmeester_to_yojson x

type bestuurslid = Bestuurslid of string
[@@deriving yojson]
let mk_bestuurslid_option row_num col_num = mk_person_option ("bestuurslid", row_num, col_num) (fun x -> Bestuurslid x)
let bestuurslid_to_yojson x = fix_json_variant bestuurslid_to_yojson x

type doelstelling = Doelstelling of string
[@@deriving yojson]
let mk_doelstelling x = Doelstelling x
let doelstelling_to_yojson x = fix_json_variant doelstelling_to_yojson x

type stichter = Stichter of string
[@@deriving yojson]
let mk_stichter x = Stichter x
let stichter_to_yojson x = fix_json_variant stichter_to_yojson x

type historie = Historie of string
[@@deriving yojson]
let mk_historie x = Historie x
let historie_to_yojson x = fix_json_variant historie_to_yojson x

type beleidsplan_op_website = BeleidsplanOpWebsite of bool
[@@deriving yojson]
let mk_beleidsplan_op_website x = BeleidsplanOpWebsite x
let beleidsplan_op_website_to_yojson x = fix_json_variant beleidsplan_op_website_to_yojson x

type doelgroep = Doelgroep of string
[@@deriving yojson]
let mk_doelgroep = function
  | "Geen specifieke doelgroep" -> None
  | x -> Some (Doelgroep x)
let doelgroep_to_yojson x = fix_json_variant doelgroep_to_yojson x

type doelgroep_overig = DoelgroepOverig of string
[@@deriving yojson]
let mk_doelgroep_overig x = DoelgroepOverig x
let doelgroep_overig_to_yojson x = fix_json_variant doelgroep_overig_to_yojson x

type activiteiten_beschrijving = ActiviteitenBeschrijving of string
[@@deriving yojson]
let mk_activiteiten_beschrijving x = ActiviteitenBeschrijving x
let activiteiten_beschrijving_to_yojson x = fix_json_variant activiteiten_beschrijving_to_yojson x

type interventie_niveau = InterventieNiveau of string
[@@deriving yojson]
let mk_interventie_niveau x = InterventieNiveau x
let interventie_niveau_to_yojson x = fix_json_variant interventie_niveau_to_yojson x

type werk_regio = WerkRegio of string
[@@deriving yojson]
let mk_werk_regio x = WerkRegio x
let werk_regio_to_yojson x = fix_json_variant werk_regio_to_yojson x

type landen = Landen of string
[@@deriving yojson]
let mk_landen x = Landen x
let landen_to_yojson x = fix_json_variant landen_to_yojson x

type regio_in_nederland = RegioInNederland of string
[@@deriving yojson]
let mk_regio_in_nederland x = RegioInNederland x
let regio_in_nederland_to_yojson x = fix_json_variant regio_in_nederland_to_yojson x

type plaats_in_nederland = PlaatsInNederland of string
[@@deriving yojson]
let mk_plaats_in_nederland x = PlaatsInNederland x
let plaats_in_nederland_to_yojson x = fix_json_variant plaats_in_nederland_to_yojson x

(* --- regio is a combination of landen, regio_in_nederland, plaats_in_nederland *)
type regio = Regio of string
[@@deriving yojson]
let regio_to_yojson x = fix_json_variant regio_to_yojson x

let split_regio s =
  let re' = Re.Perl.compile_pat "\\s*,\\s*" in
  Re.split re' s

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
let mk_besteding_budget x = BestedingBudget x
let besteding_budget_to_yojson x = fix_json_variant besteding_budget_to_yojson x

type ondersteunde_projecten = OndersteundeProjecten of string
[@@deriving yojson]
let mk_ondersteunde_projecten x = OndersteundeProjecten x
let ondersteunde_projecten_to_yojson x = fix_json_variant ondersteunde_projecten_to_yojson x

type fin_fonds = FinFonds of string
[@@deriving yojson]
let mk_fin_fonds x = FinFonds x
let fin_fonds_to_yojson x = fix_json_variant fin_fonds_to_yojson x

type max_ondersteuning = MaxOndersteuning of string
[@@deriving yojson]
let mk_max_ondersteuning x = MaxOndersteuning x
let max_ondersteuning_to_yojson x = fix_json_variant max_ondersteuning_to_yojson x

type min_ondersteuning = MinOndersteuning of string
[@@deriving yojson]
let mk_min_ondersteuning x = MinOndersteuning x
let min_ondersteuning_to_yojson x = fix_json_variant min_ondersteuning_to_yojson x

type beschrijving_project_aanmerking = BeschrijvingProjectAanmerking of string
[@@deriving yojson]
let mk_beschrijving_project_aanmerking x = BeschrijvingProjectAanmerking x
let beschrijving_project_aanmerking_to_yojson x = fix_json_variant beschrijving_project_aanmerking_to_yojson x

type doorloop_tijd_act = DoorloopTijdAct of string
[@@deriving yojson]
let mk_doorloop_tijd_act x = DoorloopTijdAct x
let doorloop_tijd_act_to_yojson x = fix_json_variant doorloop_tijd_act_to_yojson x

type fonds_type_aanvraag = FondsTypeAanvraag of string
[@@deriving yojson]
let mk_fonds_type_aanvraag x = FondsTypeAanvraag x
let fonds_type_aanvraag_to_yojson x = fix_json_variant fonds_type_aanvraag_to_yojson x

type uitsluiting = Uitsluiting of string
[@@deriving yojson]
let mk_uitsluiting x = Uitsluiting x
let uitsluiting_to_yojson x = fix_json_variant uitsluiting_to_yojson x

type op_aanvraag = OpAanvraag of bool
[@@deriving yojson]
let mk_op_aanvraag x = OpAanvraag x
let op_aanvraag_to_yojson x = fix_json_variant op_aanvraag_to_yojson x

type doorloop_tijd = DoorloopTijd of string
[@@deriving yojson]
let mk_doorloop_tijd x = DoorloopTijd x
let doorloop_tijd_to_yojson x = fix_json_variant doorloop_tijd_to_yojson x

type aanvraag_procedure = AanvraagProcedure of string
[@@deriving yojson]
let mk_aanvraag_procedure x = AanvraagProcedure x
let aanvraag_procedure_to_yojson x = fix_json_variant aanvraag_procedure_to_yojson x

type url_aanvraag_procedure = UrlAanvraagProcedure of Url.t
[@@deriving yojson]
let mk_url_aanvraag_procedure x = UrlAanvraagProcedure (url_fix_protocol x)
let url_aanvraag_procedure_to_yojson x = fix_json_variant url_aanvraag_procedure_to_yojson x

type eigen_vermogen = EigenVermogen of string
[@@deriving yojson]
let mk_eigen_vermogen x = EigenVermogen x
let eigen_vermogen_to_yojson x = fix_json_variant eigen_vermogen_to_yojson x

type inkomsten_eigen_vermogen = InkomstenEigenVermogen of string
[@@deriving yojson]
let mk_inkomsten_eigen_vermogen x = InkomstenEigenVermogen x
let inkomsten_eigen_vermogen_to_yojson x = fix_json_variant inkomsten_eigen_vermogen_to_yojson x

type herkomst_middelen = HerkomstMiddelen of string
[@@deriving yojson]
let mk_herkomst_middelen x = HerkomstMiddelen x
let herkomst_middelen_to_yojson x = fix_json_variant herkomst_middelen_to_yojson x

type boekjaar = Boekjaar of string
[@@deriving yojson]
let mk_boekjaar x = Boekjaar x
let boekjaar_to_yojson x = fix_json_variant boekjaar_to_yojson x

type url_jaarverslag = UrlJaarverslag of Url.t
[@@deriving yojson]
let mk_url_jaarverslag x = UrlJaarverslag (url_fix_protocol x)
let url_jaarverslag_to_yojson x = fix_json_variant url_jaarverslag_to_yojson x

type contact = Contact of string
[@@deriving yojson]
let mk_contact x = Contact x
let contact_to_yojson x = fix_json_variant contact_to_yojson x

type cpfinaanvragen = Cpfinaanvragen of string
[@@deriving yojson]
let mk_cpfinaanvragen_option row_num col_num = mk_person_option ("cpfinaanvragen", row_num, col_num) (fun x -> Cpfinaanvragen x)
let cpfinaanvragen_to_yojson x = fix_json_variant cpfinaanvragen_to_yojson x

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
let mk_email x = Email x
let email_to_yojson x = fix_json_variant email_to_yojson x

type telefoon = Telefoon of string
[@@deriving yojson]
let mk_telefoon x = Telefoon x
let telefoon_to_yojson x = fix_json_variant telefoon_to_yojson x

type telefoon_fin_aanvragen = TelefoonFinAanvragen of string
[@@deriving yojson]
let mk_telefoon_fin_aanvragen x = TelefoonFinAanvragen x
let telefoon_fin_aanvragen_to_yojson x = fix_json_variant telefoon_fin_aanvragen_to_yojson x

type trefwoord= Trefwoord of string
[@@deriving yojson]
let mk_trefwoord x = Trefwoord x
let trefwoord_to_yojson x = fix_json_variant trefwoord_to_yojson x
let map_trefwoord f (Trefwoord x) = Trefwoord (f x)

let mk_naam_moeder_organisatie x = NaamMoederOrganisatie x
let naam_moeder_organisatie_to_yojson x = fix_json_variant naam_moeder_organisatie_to_yojson x

let mk_url x = Url.Url x
let mk_id d = Id d
let mk_naam_organisatie x = NaamOrganisatie x
let mk_categorie x = Categorie x
let mk_website url' = Website (url_fix_protocol url')
let mk_type_organisatie x = TypeOrganisatie x

let id_to_yojson x = fix_json_variant id_to_yojson x
let naam_organisatie_to_yojson x = fix_json_variant naam_organisatie_to_yojson x
let categorie_to_yojson x = fix_json_variant categorie_to_yojson x
let website_to_yojson x = fix_json_variant website_to_yojson x
let type_organisatie_to_yojson x = fix_json_variant type_organisatie_to_yojson x

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

module Column = struct
  type 'a mk = [
    | `Bool of bool -> 'a
    | `Int of int -> 'a
    | `Text of string -> 'a
    (* | `Text of string list -> 'a *)
    (* | `Text' of string array -> 'a *)
    | `Url of Url.t -> 'a
  ]
  type 'a t = Column of { name: string; validate_pattern: string; mk: 'a mk; }
  let name (Column t) = t.name
  let validate_pattern (Column t) = t.validate_pattern
  let mk (Column t) = t.mk
end

let validate_bool = {| (Ja|ja|Nee|nee) |}
let validate_int = {| -? \d+ |}
let validate_text = {| .+ |}

let _validate_url_strict = {| (https?://)? ((\w|-)+\.)* (\w|-)+ (/ [\w \d % \( \) \. _ -]*)* (\#[\w\d%-]*)? (\? [\w \d \( \) % & = ; \. -]*)? |}
let _validate_url_full =
  {| (?:(?:(?:https?|ftp):)?\/\/)(?:\S+(?::\S*)?@)?(?: |} ^
  {| (?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?: |} ^
  "[a-z0-9\u{00a1}-\u{ffff}][a-z0-9\u{00a1}-\u{ffff}_-]{0,62})?" ^
  "[a-z0-9\u{00a1}-\u{ffff}]\\.)+(?:[a-z\u{00a1}-\u{ffff}]" ^
  {| {2,}\.?))(?::\d{2,5})?(?:[/?\#]\S*)? |}

let validate_url = _validate_url_strict

let id x = x
let mk_string = id

let bool_of_string_nl = function
  | "Ja" -> Ok true
  | "ja" -> Ok true
  | "Nee" -> Ok false
  | "nee" -> Ok false
  | x -> Error (`Msg (Fmt.str "Can't convert %s to bool" x))

let col_id = Column.Column {
  name = "id";
  validate_pattern = validate_int;
  mk = `Int mk_id;
}
let col_naam_organisatie = Column.Column {
  name = "naam_organisatie";
  validate_pattern = validate_text;
  mk = `Text mk_naam_organisatie;
}
let col_categories = Column.Column {
  name = "categories";
  validate_pattern = validate_text;
  mk = `Text (List.map (mk_categorie % String.trim) % String.split_on_char (';'))
}
let col_website = Column.Column {
  name = "website";
  validate_pattern = validate_url;
  mk = `Url mk_website;
}
let col_type_organisatie = Column.Column {
  name = "type_organisatie";
  validate_pattern = validate_text;
  mk = `Text mk_type_organisatie;
}
let col_naam_moeder_organisatie = Column.Column {
  name = "naam_moeder_organisatie";
  validate_pattern = validate_text;
  mk = `Text mk_naam_moeder_organisatie;
}
let col_oprichtings_datum = Column.Column {
  name = "oprichtings_datum";
  validate_pattern = validate_text;
  mk = `Text mk_oprichtings_datum;
}
let col_rechtsvorm = Column.Column {
  name = "rechtsvorm";
  validate_pattern = validate_text;
  mk = `Text mk_rechtsvorm;
}
let col_kvk_number = Column.Column {
  name = "kvk_number";
  validate_pattern = validate_text;
  mk = `Text mk_kvk_number;
}
let col_anbi_status = Column.Column {
  name = "anbi_status";
  validate_pattern = validate_bool;
  mk = `Bool mk_anbi_status;
}
let col_rsin = Column.Column {
  name = "rsin";
  validate_pattern = validate_int;
  mk = `Int mk_rsin;
}
let col_directeur_algemeen_geslacht = Column.Column {
  name = "directeur_algemeen_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_directeur_algemeen_voorletters = Column.Column {
  name = "directeur_algemeen_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_directeur_algemeen_tussenvoegsel = Column.Column {
  name = "directeur_algemeen_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_directeur_algemeen_achternaam = Column.Column {
  name = "directeur_algemeen_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuursvoorzitter_geslacht = Column.Column {
  name = "bestuursvoorzitter_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuursvoorzitter_voorletters = Column.Column {
  name = "bestuursvoorzitter_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuursvoorzitter_tussenvoegsel = Column.Column {
  name = "bestuursvoorzitter_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuursvoorzitter_achternaam = Column.Column {
  name = "bestuursvoorzitter_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurssecretaris_geslacht = Column.Column {
  name = "bestuurssecretaris_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurssecretaris_voorletters = Column.Column {
  name = "bestuurssecretaris_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurssecretaris_tussenvoegsel = Column.Column {
  name = "bestuurssecretaris_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurssecretaris_achternaam = Column.Column {
  name = "bestuurssecretaris_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurspenningmeester_geslacht = Column.Column {
  name = "bestuurspenningmeester_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurspenningmeester_voorletters = Column.Column {
  name = "bestuurspenningmeester_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurspenningmeester_tussenvoegsel = Column.Column {
  name = "bestuurspenningmeester_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurspenningmeester_achternaam = Column.Column {
  name = "bestuurspenningmeester_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid_geslacht = Column.Column {
  name = "bestuurslid_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid_voorletters = Column.Column {
  name = "bestuurslid_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid_tussenvoegsel = Column.Column {
  name = "bestuurslid_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid_achternaam = Column.Column {
  name = "bestuurslid_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid3_geslacht = Column.Column {
  name = "bestuurslid3_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid3_voorletters = Column.Column {
  name = "bestuurslid3_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid3_tussenvoegsel = Column.Column {
  name = "bestuurslid3_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid3_achternaam = Column.Column {
  name = "bestuurslid3_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid4_geslacht = Column.Column {
  name = "bestuurslid4_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid4_voorletters = Column.Column {
  name = "bestuurslid4_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid4_tussenvoegsel = Column.Column {
  name = "bestuurslid4_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid4_achternaam = Column.Column {
  name = "bestuurslid4_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid5_geslacht = Column.Column {
  name = "bestuurslid5_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid5_voorletters = Column.Column {
  name = "bestuurslid5_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid5_tussenvoegsel = Column.Column {
  name = "bestuurslid5_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid5_achternaam = Column.Column {
  name = "bestuurslid5_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid6_geslacht = Column.Column {
  name = "bestuurslid6_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid6_voorletters = Column.Column {
  name = "bestuurslid6_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid6_tussenvoegsel = Column.Column {
  name = "bestuurslid6_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_bestuurslid6_achternaam = Column.Column {
  name = "bestuurslid6_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_doelstelling = Column.Column {
  name = "doelstelling";
  validate_pattern = validate_text;
  mk = `Text mk_doelstelling;
}
let col_stichter = Column.Column {
  name = "stichter";
  validate_pattern = validate_text;
  mk = `Text mk_stichter;
}
let col_historie = Column.Column {
  name = "historie";
  validate_pattern = validate_text;
  mk = `Text mk_historie;
}
let col_beleidsplan_op_website = Column.Column {
  name = "beleidsplan_op_website";
  validate_pattern = validate_text;
  mk = `Bool mk_beleidsplan_op_website;
}
let col_doelgroep = Column.Column {
  name = "doelgroep";
  validate_pattern = validate_text;
  mk = `Text mk_doelgroep;
}
let col_doelgroep_overig = Column.Column {
  name = "doelgroep_overig";
  validate_pattern = validate_text;
  mk = `Text mk_doelgroep_overig;
}
let col_activiteiten_beschrijving = Column.Column {
  name = "activiteiten_beschrijving";
  validate_pattern = validate_text;
  mk = `Text mk_activiteiten_beschrijving;
}
let col_interventie_niveau = Column.Column {
  name = "interventie_niveau";
  validate_pattern = validate_text;
  mk = `Text mk_interventie_niveau;
}
let col_werk_regio = Column.Column {
  name = "werk_regio";
  validate_pattern = validate_text;
  mk = `Text mk_werk_regio;
}
let col_landen = Column.Column {
  name = "landen";
  validate_pattern = validate_text;
  mk = `Text mk_landen;
}
let col_regio_in_nederland = Column.Column {
  name = "regio_in_nederland";
  validate_pattern = validate_text;
  mk = `Text mk_regio_in_nederland;
}
let col_plaats_in_nederland = Column.Column {
  name = "plaats_in_nederland";
  validate_pattern = validate_text;
  mk = `Text mk_plaats_in_nederland;
}
let col_besteding_budget = Column.Column {
  name = "besteding_budget";
  validate_pattern = validate_text;
  mk = `Text mk_besteding_budget;
}
let col_ondersteunde_projecten = Column.Column {
  name = "ondersteunde_projecten";
  validate_pattern = validate_text;
  mk = `Text mk_ondersteunde_projecten;
}
let col_fin_fonds = Column.Column {
  name = "fin_fonds";
  validate_pattern = validate_text;
  mk = `Text mk_fin_fonds;
}
let col_max_ondersteuning = Column.Column {
  name = "max_ondersteuning";
  validate_pattern = validate_text;
  mk = `Text mk_max_ondersteuning;
}
let col_min_ondersteuning = Column.Column {
  name = "min_ondersteuning";
  validate_pattern = validate_text;
  mk = `Text mk_min_ondersteuning;
}
let col_beschrijving_project_aanmerking = Column.Column {
  name = "beschrijving_project_aanmerking";
  validate_pattern = validate_text;
  mk = `Text mk_beschrijving_project_aanmerking;
}
let col_doorloop_tijd_act = Column.Column {
  name = "doorloop_tijd_act";
  validate_pattern = validate_text;
  mk = `Text mk_doorloop_tijd_act;
}
let col_fonds_type_aanvraag = Column.Column {
  name = "fonds_type_aanvraag";
  validate_pattern = validate_text;
  mk = `Text mk_fonds_type_aanvraag;
}
let col_uitsluiting = Column.Column {
  name = "uitsluiting";
  validate_pattern = validate_text;
  mk = `Text mk_uitsluiting;
}
let col_op_aanvraag = Column.Column {
  name = "op_aanvraag";
  validate_pattern = validate_text;
  mk = `Bool mk_op_aanvraag;
}
let col_doorloop_tijd = Column.Column {
  name = "doorloop_tijd";
  validate_pattern = validate_text;
  mk = `Text mk_doorloop_tijd;
}
let col_aanvraag_procedure = Column.Column {
  name = "aanvraag_procedure";
  validate_pattern = validate_text;
  mk = `Text mk_aanvraag_procedure;
}
let col_url_aanvraag_procedure = Column.Column {
  name = "url_aanvraag_procedure";
  validate_pattern = validate_text;
  mk = `Url mk_url_aanvraag_procedure;
}
let col_eigen_vermogen = Column.Column {
  name = "eigen_vermogen";
  validate_pattern = validate_text;
  mk = `Text mk_eigen_vermogen;
}
let col_inkomsten_eigen_vermogen = Column.Column {
  name = "inkomsten_eigen_vermogen";
  validate_pattern = validate_text;
  mk = `Text mk_inkomsten_eigen_vermogen;
}
let col_herkomst_middelen = Column.Column {
  name = "herkomst_middelen";
  validate_pattern = validate_text;
  mk = `Text mk_herkomst_middelen;
}
let col_boekjaar = Column.Column {
  name = "boekjaar";
  validate_pattern = validate_text;
  mk = `Text mk_boekjaar;
}
let col_url_jaarverslag = Column.Column {
  name = "url_jaarverslag";
  validate_pattern = validate_text;
  mk = `Url mk_url_jaarverslag;
}
let col_contact = Column.Column {
  name = "contact";
  validate_pattern = validate_text;
  mk = `Text mk_contact;
}
let col_cpfinaanvragen_geslacht = Column.Column {
  name = "cpfinaanvragen_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_cpfinaanvragen_voorletters = Column.Column {
  name = "cpfinaanvragen_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_cpfinaanvragen_tussenvoegsel = Column.Column {
  name = "cpfinaanvragen_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_cpfinaanvragen_achternaam = Column.Column {
  name = "cpfinaanvragen_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_postadres_straat = Column.Column {
  name = "postadres_straat";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_postadres_huisnummer = Column.Column {
  name = "postadres_huisnummer";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_postadres_huisnummer_ext = Column.Column {
  name = "postadres_huisnummer_ext";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_postadres_postcode = Column.Column {
  name = "postadres_postcode";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_postadres_plaats = Column.Column {
  name = "postadres_plaats";
  validate_pattern = validate_text;
  mk = `Text mk_string;
}
let col_email = Column.Column {
  name = "email";
  validate_pattern = validate_text;
  mk = `Text mk_email;
}
let col_telefoon = Column.Column {
  name = "telefoon";
  validate_pattern = validate_text;
  mk = `Text mk_telefoon;
}
let col_telefoon_fin_aanvragen = Column.Column {
  name = "telefoon_fin_aanvragen";
  validate_pattern = validate_text;
  mk = `Text mk_telefoon_fin_aanvragen;
}
let col_trefwoorden = Column.Column {
  name = "trefwoorden";
  validate_pattern = validate_text;
  mk = `Text (List.map (mk_trefwoord % String.trim) % String.split_on_char (','))
}
