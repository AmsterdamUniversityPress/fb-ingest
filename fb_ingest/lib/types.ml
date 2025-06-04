let (let*) = Result.bind
let (let+) = fun a b -> Result.map b a
let (>>=) = Result.bind

let num_cols = 39
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
      |> List.map (mk_categorie % Util.String.trim)
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
  (* Result.map (fun str' -> ...) (validate_text str) *)
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

type directeur = Directeur of string
[@@deriving yojson]

let directeur_to_yojson x = fix_json_variant directeur_to_yojson x
let mk_directeur_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Directeur str)

type bestuursvoorzitter = Bestuursvoorzitter  of string
[@@deriving yojson]
let mk_bestuursvoorzitter_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Bestuursvoorzitter str)
let bestuursvoorzitter_to_yojson x = fix_json_variant bestuursvoorzitter_to_yojson x

type bestuurssecretaris = Bestuurssecretaris of string
[@@deriving yojson]

let mk_bestuurssecretaris_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Bestuurssecretaris str)

let bestuurssecretaris_to_yojson x = fix_json_variant bestuurssecretaris_to_yojson x

type penningmeester = Penningmeester of string
[@@deriving yojson]
let mk_penningmeester_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Penningmeester str)

let penningmeester_to_yojson x = fix_json_variant penningmeester_to_yojson x

type doelstelling = Doelstelling of string
[@@deriving yojson]

let mk_doelstelling_r str =
  str
  |> Util.String.trim
  |> validate_text
  |> Result.map (fun str -> Doelstelling str)

let doelstelling_to_yojson x = fix_json_variant doelstelling_to_yojson x

type doelgroep = Doelgroep of string
[@@deriving yojson]

let mk_doelgroep_r str =
  str
  |> validate_text
  (* |> Result.map (fun str -> Some (Doelgroep str)) *)
  |> Result.map (fun str -> Doelgroep str)

let doelgroep_to_yojson x = fix_json_variant doelgroep_to_yojson x

type activiteiten = Activiteiten of string
[@@deriving yojson]

let mk_activiteiten_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Activiteiten str)

let activiteiten_to_yojson x = fix_json_variant activiteiten_to_yojson x

type werkterrein_geografisch = Werkterrein_geografisch of string
[@@deriving yojson]

let mk_werkterreinen_geografisch_r str =
  str
  |> validate_text
  |> Result.map (fun str ->
      str
      |> String.split_on_char ';'
      |> List.map (fun s -> Werkterrein_geografisch (String.trim s)))

let werkterrein_geografisch_to_yojson x = fix_json_variant werkterrein_geografisch_to_yojson x

type contact = Contact of string
[@@deriving yojson]

let mk_contact_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Contact str)

let contact_to_yojson x = fix_json_variant contact_to_yojson x

let mk_postadres_straat_r str = str |> validate_text
let mk_postadres_huisnummer_r str = str |> validate_int
let mk_postadres_huisnummer_ext_r str = str |> validate_text
let mk_postadres_postcode_r str = str |> validate_text
let mk_postadres_plaats_r str = str |> validate_text

type postadres = Postadres of string
[@@deriving yojson]

let postadres_to_yojson = fix_json_variant postadres_to_yojson

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

type trefwoord = Trefwoord of string
[@@deriving yojson]
let mk_trefwoorden_r str =
  str
  |> validate_text
  |> Result.map (fun str ->
      str
      |> String.split_on_char ';'
      |> List.map (fun s -> Trefwoord (String.trim s)))

let trefwoord_to_yojson x = fix_json_variant trefwoord_to_yojson x

type facebook = Facebook of Url.t
[@@deriving yojson]

let mk_facebook_r str =
  str
  |> validate_url
  |> Result.map (fun str ->
      str
      |> mk_url
      |> url_fix_protocol
      |> fun url -> Facebook url
    )

let facebook_to_yojson x = fix_json_variant facebook_to_yojson x

type linkedin = LinkedIn of Url.t
[@@deriving yojson]

let mk_linkedin_r str =
  str
  |> validate_url
  |> Result.map (fun str ->
      str
      |> mk_url
      |> url_fix_protocol
      |> fun url -> LinkedIn url
    )

let linkedin_to_yojson x = fix_json_variant linkedin_to_yojson x

type instagram = Instagram of Url.t
[@@deriving yojson]

let mk_instagram_r str =
  str
  |> validate_url
  |> Result.map (fun str ->
      str
      |> mk_url
      |> url_fix_protocol
      |> fun url -> Instagram url
    )

let instagram_to_yojson x = fix_json_variant instagram_to_yojson x

type twitter = Twitter of Url.t
[@@deriving yojson]

let mk_twitter_r str =
  str
  |> validate_url
  |> Result.map (fun str ->
      str
      |> mk_url
      |> url_fix_protocol
      |> fun url -> Twitter url
    )

let twitter_to_yojson x = fix_json_variant twitter_to_yojson x

type bijzonderheden = Bijzonderheden of string
[@@deriving yojson]

let mk_bijzonderheden_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Bijzonderheden str)

let bijzonderheden_to_yojson x = fix_json_variant bijzonderheden_to_yojson x

type opmerkingen = Opmerkingen of string
[@@deriving yojson]

let mk_opmerkingen_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Opmerkingen str)

let opmerkingen_to_yojson x = fix_json_variant opmerkingen_to_yojson x

type aanvraagprocedure = Aanvraagprocedure of string
[@@deriving yojson]

let mk_aanvraagprocedure_r str =
  str
  |> validate_text
  |> Result.map (fun str -> Aanvraagprocedure str)

let aanvraagprocedure_to_yojson x = fix_json_variant aanvraagprocedure_to_yojson x

type fonds = Fonds of {
  id: id;
  uuid: string;
  naam_organisatie: naam_organisatie;
  categories: categorie list;
  website: website option;
  tweede_website: website option;
  type_organisatie: type_organisatie option;
  naam_moeder_organisatie: naam_moeder_organisatie option;
  rechtsvorm: rechtsvorm;
  kvk_number: kvk_number option;
  anbi_status: anbi_status;
  rsin: rsin option;
  directeur: directeur option;
  bestuursvoorzitter: bestuursvoorzitter option;
  bestuurssecretaris: bestuurssecretaris option;
  penningmeester: penningmeester option;
  doelstelling: doelstelling option;
  doelgroep: doelgroep option;
  activiteiten: activiteiten option;
  werkterreinen_geografisch: werkterrein_geografisch list;
  aanvraagprocedure: aanvraagprocedure option;
  contact: contact option;
  postadres: postadres option;
  email: email option;
  telefoon: telefoon option;
  trefwoorden: trefwoord list;
  facebook: facebook option;
  linkedin: linkedin option;
  instagram: instagram option;
  twitter: twitter option;
  bijzonderheden: bijzonderheden option;
  bijzonderheden2: bijzonderheden option;
  bijzonderheden3: bijzonderheden option;
  opmerkingen: opmerkingen option;
}
[@@deriving yojson]

let fonds_to_yojson x = fix_json_variant fonds_to_yojson x

type t = fonds list
[@@deriving yojson]

let to_json_string = Yojson.Safe.to_string % to_yojson
