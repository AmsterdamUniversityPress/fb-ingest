let num_cols = 48

(* --- this sucks a bit -- deriving yojson makes `Id of string` into ["Id", "xxx"], and we just want "xxx". So we need to use this on every type which has a constructor. *)

let fix_json_variant f x =
  match f x with
  | `List xs -> List.nth xs 1
  | _ -> failwith "unexpected: fix_json_variant got a non-list"

type url = Url of string
[@@deriving yojson]

let url_to_yojson x = fix_json_variant url_to_yojson x

type id = Id of int
[@@deriving yojson]
type naam_organisatie = NaamOrganisatie of string
[@@deriving yojson]
type categorie = Categorie of string
[@@deriving yojson]
type website = Website of url
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

type directeur_algemeen_geslacht = DirecteurAlgemeenGeslacht of string
[@@deriving yojson]
let mk_directeur_algemeen_geslacht x = DirecteurAlgemeenGeslacht x
let directeur_algemeen_geslacht_to_yojson x = fix_json_variant directeur_algemeen_geslacht_to_yojson x

type directeur_algemeen_voorletters = DirecteurAlgemeenVoorletters of string
[@@deriving yojson]
let mk_directeur_algemeen_voorletters x = DirecteurAlgemeenVoorletters x
let directeur_algemeen_voorletters_to_yojson x = fix_json_variant directeur_algemeen_voorletters_to_yojson x

type directeur_algemeen_tussenvoegsel = DirecteurAlgemeenTussenvoegsel of string
[@@deriving yojson]
let mk_directeur_algemeen_tussenvoegsel x = DirecteurAlgemeenTussenvoegsel x
let directeur_algemeen_tussenvoegsel_to_yojson x = fix_json_variant directeur_algemeen_tussenvoegsel_to_yojson x

type directeur_algemeen_achternaam = DirecteurAlgemeenAchternaam of string
[@@deriving yojson]
let mk_directeur_algemeen_achternaam x = DirecteurAlgemeenAchternaam x
let directeur_algemeen_achternaam_to_yojson x = fix_json_variant directeur_algemeen_achternaam_to_yojson x

type bestuursvoorzitter_geslacht = BestuursvoorzitterGeslacht of string
[@@deriving yojson]
let mk_bestuursvoorzitter_geslacht x = BestuursvoorzitterGeslacht x
let bestuursvoorzitter_geslacht_to_yojson x = fix_json_variant bestuursvoorzitter_geslacht_to_yojson x

type bestuursvoorzitter_voorletters = BestuursvoorzitterVoorletters of string
[@@deriving yojson]
let mk_bestuursvoorzitter_voorletters x = BestuursvoorzitterVoorletters x
let bestuursvoorzitter_voorletters_to_yojson x = fix_json_variant bestuursvoorzitter_voorletters_to_yojson x

type bestuursvoorzitter_tussenvoegsel = BestuursvoorzitterTussenvoegsel of string
[@@deriving yojson]
let mk_bestuursvoorzitter_tussenvoegsel x = BestuursvoorzitterTussenvoegsel x
let bestuursvoorzitter_tussenvoegsel_to_yojson x = fix_json_variant bestuursvoorzitter_tussenvoegsel_to_yojson x

type bestuursvoorzitter_achternaam = BestuursvoorzitterAchternaam of string
[@@deriving yojson]
let mk_bestuursvoorzitter_achternaam x = BestuursvoorzitterAchternaam x
let bestuursvoorzitter_achternaam_to_yojson x = fix_json_variant bestuursvoorzitter_achternaam_to_yojson x

type bestuurssecretaris_geslacht = BestuurssecretarisGeslacht of string
[@@deriving yojson]
let mk_bestuurssecretaris_geslacht x = BestuurssecretarisGeslacht x
let bestuurssecretaris_geslacht_to_yojson x = fix_json_variant bestuurssecretaris_geslacht_to_yojson x

type bestuurssecretaris_voorletters = BestuurssecretarisVoorletters of string
[@@deriving yojson]
let mk_bestuurssecretaris_voorletters x = BestuurssecretarisVoorletters x
let bestuurssecretaris_voorletters_to_yojson x = fix_json_variant bestuurssecretaris_voorletters_to_yojson x

type bestuurssecretaris_tussenvoegsel = BestuurssecretarisTussenvoegsel of string
[@@deriving yojson]
let mk_bestuurssecretaris_tussenvoegsel x = BestuurssecretarisTussenvoegsel x
let bestuurssecretaris_tussenvoegsel_to_yojson x = fix_json_variant bestuurssecretaris_tussenvoegsel_to_yojson x

type bestuurssecretaris_achternaam = BestuurssecretarisAchternaam of string
[@@deriving yojson]
let mk_bestuurssecretaris_achternaam x = BestuurssecretarisAchternaam x
let bestuurssecretaris_achternaam_to_yojson x = fix_json_variant bestuurssecretaris_achternaam_to_yojson x

type bestuurspenningmeester_geslacht = BestuurspenningmeesterGeslacht of string
[@@deriving yojson]
let mk_bestuurspenningmeester_geslacht x = BestuurspenningmeesterGeslacht x
let bestuurspenningmeester_geslacht_to_yojson x = fix_json_variant bestuurspenningmeester_geslacht_to_yojson x

type bestuurspenningmeester_voorletters = BestuurspenningmeesterVoorletters of string
[@@deriving yojson]
let mk_bestuurspenningmeester_voorletters x = BestuurspenningmeesterVoorletters x
let bestuurspenningmeester_voorletters_to_yojson x = fix_json_variant bestuurspenningmeester_voorletters_to_yojson x

type bestuurspenningmeester_tussenvoegsel = BestuurspenningmeesterTussenvoegsel of string
[@@deriving yojson]
let mk_bestuurspenningmeester_tussenvoegsel x = BestuurspenningmeesterTussenvoegsel x
let bestuurspenningmeester_tussenvoegsel_to_yojson x = fix_json_variant bestuurspenningmeester_tussenvoegsel_to_yojson x

type bestuurspenningmeester_achternaam = BestuurspenningmeesterAchternaam of string
[@@deriving yojson]
let mk_bestuurspenningmeester_achternaam x = BestuurspenningmeesterAchternaam x
let bestuurspenningmeester_achternaam_to_yojson x = fix_json_variant bestuurspenningmeester_achternaam_to_yojson x

type bestuurslid3_geslacht = Bestuurslid3Geslacht of string
[@@deriving yojson]
let mk_bestuurslid3_geslacht x = Bestuurslid3Geslacht x
let bestuurslid3_geslacht_to_yojson x = fix_json_variant bestuurslid3_geslacht_to_yojson x

type bestuurslid3_voorletters = Bestuurslid3Voorletters of string
[@@deriving yojson]
let mk_bestuurslid3_voorletters x = Bestuurslid3Voorletters x
let bestuurslid3_voorletters_to_yojson x = fix_json_variant bestuurslid3_voorletters_to_yojson x

type bestuurslid3_tussenvoegsel = Bestuurslid3Tussenvoegsel of string
[@@deriving yojson]
let mk_bestuurslid3_tussenvoegsel x = Bestuurslid3Tussenvoegsel x
let bestuurslid3_tussenvoegsel_to_yojson x = fix_json_variant bestuurslid3_tussenvoegsel_to_yojson x

type bestuurslid3_achternaam = Bestuurslid3Achternaam of string
[@@deriving yojson]
let mk_bestuurslid3_achternaam x = Bestuurslid3Achternaam x
let bestuurslid3_achternaam_to_yojson x = fix_json_variant bestuurslid3_achternaam_to_yojson x

type bestuurslid4_geslacht = Bestuurslid4Geslacht of string
[@@deriving yojson]
let mk_bestuurslid4_geslacht x = Bestuurslid4Geslacht x
let bestuurslid4_geslacht_to_yojson x = fix_json_variant bestuurslid4_geslacht_to_yojson x

type bestuurslid4_voorletters = Bestuurslid4Voorletters of string
[@@deriving yojson]
let mk_bestuurslid4_voorletters x = Bestuurslid4Voorletters x
let bestuurslid4_voorletters_to_yojson x = fix_json_variant bestuurslid4_voorletters_to_yojson x

type bestuurslid4_tussenvoegsel = Bestuurslid4Tussenvoegsel of string
[@@deriving yojson]
let mk_bestuurslid4_tussenvoegsel x = Bestuurslid4Tussenvoegsel x
let bestuurslid4_tussenvoegsel_to_yojson x = fix_json_variant bestuurslid4_tussenvoegsel_to_yojson x

type bestuurslid4_achternaam = Bestuurslid4Achternaam of string
[@@deriving yojson]
let mk_bestuurslid4_achternaam x = Bestuurslid4Achternaam x
let bestuurslid4_achternaam_to_yojson x = fix_json_variant bestuurslid4_achternaam_to_yojson x

type bestuurslid5_geslacht = Bestuurslid5Geslacht of string
[@@deriving yojson]
let mk_bestuurslid5_geslacht x = Bestuurslid5Geslacht x
let bestuurslid5_geslacht_to_yojson x = fix_json_variant bestuurslid5_geslacht_to_yojson x

type bestuurslid5_voorletters = Bestuurslid5Voorletters of string
[@@deriving yojson]
let mk_bestuurslid5_voorletters x = Bestuurslid5Voorletters x
let bestuurslid5_voorletters_to_yojson x = fix_json_variant bestuurslid5_voorletters_to_yojson x

type bestuurslid5_tussenvoegsel = Bestuurslid5Tussenvoegsel of string
[@@deriving yojson]
let mk_bestuurslid5_tussenvoegsel x = Bestuurslid5Tussenvoegsel x
let bestuurslid5_tussenvoegsel_to_yojson x = fix_json_variant bestuurslid5_tussenvoegsel_to_yojson x

type bestuurslid5_achternaam = Bestuurslid5Achternaam of string
[@@deriving yojson]
let mk_bestuurslid5_achternaam x = Bestuurslid5Achternaam x
let bestuurslid5_achternaam_to_yojson x = fix_json_variant bestuurslid5_achternaam_to_yojson x

type bestuurslid6_geslacht = Bestuurslid6Geslacht of string
[@@deriving yojson]
let mk_bestuurslid6_geslacht x = Bestuurslid6Geslacht x
let bestuurslid6_geslacht_to_yojson x = fix_json_variant bestuurslid6_geslacht_to_yojson x

type bestuurslid6_voorletters = Bestuurslid6Voorletters of string
[@@deriving yojson]
let mk_bestuurslid6_voorletters x = Bestuurslid6Voorletters x
let bestuurslid6_voorletters_to_yojson x = fix_json_variant bestuurslid6_voorletters_to_yojson x

type bestuurslid6_tussenvoegsel = Bestuurslid6Tussenvoegsel of string
[@@deriving yojson]
let mk_bestuurslid6_tussenvoegsel x = Bestuurslid6Tussenvoegsel x
let bestuurslid6_tussenvoegsel_to_yojson x = fix_json_variant bestuurslid6_tussenvoegsel_to_yojson x

type bestuurslid6_achternaam = Bestuurslid6Achternaam of string
[@@deriving yojson]
let mk_bestuurslid6_achternaam x = Bestuurslid6Achternaam x
let bestuurslid6_achternaam_to_yojson x = fix_json_variant bestuurslid6_achternaam_to_yojson x

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

let mk_naam_moeder_organisatie x = NaamMoederOrganisatie x
let naam_moeder_organisatie_to_yojson x = fix_json_variant naam_moeder_organisatie_to_yojson x

let mk_url x = Url x
let mk_id d = Id d
let mk_naam_organisatie x = NaamOrganisatie x
let mk_categorie x = Categorie x
let mk_website x = Website x
let mk_type_organisatie x = TypeOrganisatie x

let id_to_yojson x = fix_json_variant id_to_yojson x
let naam_organisatie_to_yojson x = fix_json_variant naam_organisatie_to_yojson x
let categorie_to_yojson x = fix_json_variant categorie_to_yojson x
let website_to_yojson x = fix_json_variant website_to_yojson x
let type_organisatie_to_yojson x = fix_json_variant type_organisatie_to_yojson x

type fonds = Fonds of {
  id: id;
  naam_organisatie: naam_organisatie;
  categorie: categorie;
  website: website;
  type_organisatie: type_organisatie option;
  naam_moeder_organisatie: naam_moeder_organisatie option;
  oprichtings_datum: oprichtings_datum;
  rechtsvorm: rechtsvorm;
  kvk_number: kvk_number;
  anbi_status: anbi_status;
  rsin: rsin;
  directeur_algemeen_geslacht: directeur_algemeen_geslacht option;
  directeur_algemeen_voorletters: directeur_algemeen_voorletters option;
  directeur_algemeen_tussenvoegsel: directeur_algemeen_tussenvoegsel option;
  directeur_algemeen_achternaam: directeur_algemeen_achternaam option;
  bestuursvoorzitter_geslacht: bestuursvoorzitter_geslacht option;
  bestuursvoorzitter_voorletters: bestuursvoorzitter_voorletters option;
  bestuursvoorzitter_tussenvoegsel: bestuursvoorzitter_tussenvoegsel option;
  bestuursvoorzitter_achternaam: bestuursvoorzitter_achternaam option;
  bestuurssecretaris_geslacht: bestuurssecretaris_geslacht option;
  bestuurssecretaris_voorletters: bestuurssecretaris_voorletters option;
  bestuurssecretaris_tussenvoegsel: bestuurssecretaris_tussenvoegsel option;
  bestuurssecretaris_achternaam: bestuurssecretaris_achternaam option;
  bestuurspenningmeester_geslacht: bestuurspenningmeester_geslacht option;
  bestuurspenningmeester_voorletters: bestuurspenningmeester_voorletters option;
  bestuurspenningmeester_tussenvoegsel: bestuurspenningmeester_tussenvoegsel option;
  bestuurspenningmeester_achternaam: bestuurspenningmeester_achternaam option;
  bestuurslid3_geslacht: bestuurslid3_geslacht option;
  bestuurslid3_voorletters: bestuurslid3_voorletters option;
  bestuurslid3_tussenvoegsel: bestuurslid3_tussenvoegsel option;
  bestuurslid3_achternaam: bestuurslid3_achternaam option;
  bestuurslid4_geslacht: bestuurslid4_geslacht option;
  bestuurslid4_voorletters: bestuurslid4_voorletters option;
  bestuurslid4_tussenvoegsel: bestuurslid4_tussenvoegsel option;
  bestuurslid4_achternaam: bestuurslid4_achternaam option;
  bestuurslid5_geslacht: bestuurslid5_geslacht option;
  bestuurslid5_voorletters: bestuurslid5_voorletters option;
  bestuurslid5_tussenvoegsel: bestuurslid5_tussenvoegsel option;
  bestuurslid5_achternaam: bestuurslid5_achternaam option;
  bestuurslid6_geslacht: bestuurslid6_geslacht option;
  bestuurslid6_voorletters: bestuurslid6_voorletters option;
  bestuurslid6_tussenvoegsel: bestuurslid6_tussenvoegsel option;
  bestuurslid6_achternaam: bestuurslid6_achternaam option;
  doelstelling: doelstelling;
  stichter: stichter option;
  historie: historie option;
  beleidsplan_op_website: beleidsplan_op_website;
  doelgroep: doelgroep option;
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
    (* | `Text' of string array -> 'a *)
    | `Url of url -> 'a
  ]
  type 'a t = Column of { name: string; validate_pattern: string; mk: 'a mk; }
  let name (Column t) = t.name
  let validate_pattern (Column t) = t.validate_pattern
  let mk (Column t) = t.mk
end

let validate_text = {| .+ |}
let validate_bool = {| (?:Ja|ja|Nee|nee) |}
let validate_url = {| (https?://)? ((\w|-|\&)+\.)* \w+ (/[\w\d]+)* (/? [\w\d-_\.]+)? (\? [\w \d & = ; ]*)? |}

let bool_of_string_nl = function
  | "Ja" -> true
  | "ja" -> true
  | "Nee" -> false
  | "nee" -> false
  | x -> failwith (Fmt.str "Can't convert %s to bool" x)

let col_id = Column.Column {
  name = "id";
  validate_pattern = {| \d+ |};
  mk = `Int mk_id;
}
let col_naam_organisatie = Column.Column {
  name = "naam_organisatie";
  validate_pattern = validate_text;
  mk = `Text mk_naam_organisatie;
}
let col_categorie = Column.Column {
  name = "categorie";
  validate_pattern = validate_text;
  mk = `Text mk_categorie;
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
  validate_pattern = validate_text;
  mk = `Int mk_rsin;
}
let col_directeur_algemeen_geslacht = Column.Column {
  name = "directeur_algemeen_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_directeur_algemeen_geslacht;
}
let col_directeur_algemeen_voorletters = Column.Column {
  name = "directeur_algemeen_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_directeur_algemeen_voorletters;
}
let col_directeur_algemeen_tussenvoegsel = Column.Column {
  name = "directeur_algemeen_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_directeur_algemeen_tussenvoegsel;
}
let col_directeur_algemeen_achternaam = Column.Column {
  name = "directeur_algemeen_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_directeur_algemeen_achternaam;
}
let col_bestuursvoorzitter_geslacht = Column.Column {
  name = "bestuursvoorzitter_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_bestuursvoorzitter_geslacht;
}
let col_bestuursvoorzitter_voorletters = Column.Column {
  name = "bestuursvoorzitter_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_bestuursvoorzitter_voorletters;
}
let col_bestuursvoorzitter_tussenvoegsel = Column.Column {
  name = "bestuursvoorzitter_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_bestuursvoorzitter_tussenvoegsel;
}
let col_bestuursvoorzitter_achternaam = Column.Column {
  name = "bestuursvoorzitter_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_bestuursvoorzitter_achternaam;
}
let col_bestuurssecretaris_geslacht = Column.Column {
  name = "bestuurssecretaris_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurssecretaris_geslacht;
}
let col_bestuurssecretaris_voorletters = Column.Column {
  name = "bestuurssecretaris_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurssecretaris_voorletters;
}
let col_bestuurssecretaris_tussenvoegsel = Column.Column {
  name = "bestuurssecretaris_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurssecretaris_tussenvoegsel;
}
let col_bestuurssecretaris_achternaam = Column.Column {
  name = "bestuurssecretaris_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurssecretaris_achternaam;
}
let col_bestuurspenningmeester_geslacht = Column.Column {
  name = "bestuurspenningmeester_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurspenningmeester_geslacht;
}
let col_bestuurspenningmeester_voorletters = Column.Column {
  name = "bestuurspenningmeester_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurspenningmeester_voorletters;
}
let col_bestuurspenningmeester_tussenvoegsel = Column.Column {
  name = "bestuurspenningmeester_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurspenningmeester_tussenvoegsel;
}
let col_bestuurspenningmeester_achternaam = Column.Column {
  name = "bestuurspenningmeester_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurspenningmeester_achternaam;
}
let col_bestuurslid3_geslacht = Column.Column {
  name = "bestuurslid3_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid3_geslacht;
}
let col_bestuurslid3_voorletters = Column.Column {
  name = "bestuurslid3_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid3_voorletters;
}
let col_bestuurslid3_tussenvoegsel = Column.Column {
  name = "bestuurslid3_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid3_tussenvoegsel;
}
let col_bestuurslid3_achternaam = Column.Column {
  name = "bestuurslid3_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid3_achternaam;
}
let col_bestuurslid4_geslacht = Column.Column {
  name = "bestuurslid4_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid4_geslacht;
}
let col_bestuurslid4_voorletters = Column.Column {
  name = "bestuurslid4_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid4_voorletters;
}
let col_bestuurslid4_tussenvoegsel = Column.Column {
  name = "bestuurslid4_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid4_tussenvoegsel;
}
let col_bestuurslid4_achternaam = Column.Column {
  name = "bestuurslid4_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid4_achternaam;
}
let col_bestuurslid5_geslacht = Column.Column {
  name = "bestuurslid5_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid5_geslacht;
}
let col_bestuurslid5_voorletters = Column.Column {
  name = "bestuurslid5_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid5_voorletters;
}
let col_bestuurslid5_tussenvoegsel = Column.Column {
  name = "bestuurslid5_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid5_tussenvoegsel;
}
let col_bestuurslid5_achternaam = Column.Column {
  name = "bestuurslid5_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid5_achternaam;
}
let col_bestuurslid6_geslacht = Column.Column {
  name = "bestuurslid6_geslacht";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid6_geslacht;
}
let col_bestuurslid6_voorletters = Column.Column {
  name = "bestuurslid6_voorletters";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid6_voorletters;
}
let col_bestuurslid6_tussenvoegsel = Column.Column {
  name = "bestuurslid6_tussenvoegsel";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid6_tussenvoegsel;
}
let col_bestuurslid6_achternaam = Column.Column {
  name = "bestuurslid6_achternaam";
  validate_pattern = validate_text;
  mk = `Text mk_bestuurslid6_achternaam;
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
