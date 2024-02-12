let num_cols = 5

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
}
[@@deriving yojson]

let fonds_to_yojson x = fix_json_variant fonds_to_yojson x

type t = fonds list
[@@deriving yojson]

let to_json_string = Yojson.Safe.to_string % to_yojson

module Column = struct
  type 'a mk = [`Default of string -> 'a]
  type 'a t = Column of { name: string; validate_pattern: string; mk: 'a mk; }
  let name (Column t) = t.name
  let validate_pattern (Column t) = t.validate_pattern
  let mk (Column t) = t.mk
end

let validate_text = {| .+ |}

let col_id = Column.Column {
  name = "id";
  validate_pattern = {| \d+ |};
  mk = `Default (mk_id % int_of_string);
}
let col_naam_organisatie = Column.Column {
  name = "naam_organisatie";
  validate_pattern = validate_text;
  mk = `Default mk_naam_organisatie;
}
let col_categorie = Column.Column {
  name = "categorie";
  validate_pattern = validate_text;
  mk = `Default mk_categorie;
}
let col_website = Column.Column {
  name = "website";
  validate_pattern = {| (https?://)? ((\w|-|\&)+\.)* \w+ (/[\w\d]+)* (/? [\w\d-_\.]+)? (\? [\w \d & = ; ]*)? |};
  mk = `Default (mk_website % mk_url);
}
let col_type_organisatie = Column.Column {
  name = "type_organisatie";
  validate_pattern = validate_text;
  mk = `Default mk_type_organisatie;
}
