(* --- usage: `xs $ n` for `n`th element of a list *)
let ($) = fun xs n -> List.nth xs n

module RealString = String

module String = struct
  let strip_spaces s =
    let f = function
      | ' ' -> Seq.empty
      | x -> Seq.return x in
    String.to_seq s
    |> Seq.flat_map f
    |> String.of_seq
  (* Trims both space and U000A (no-break space). *)
  let trim x =
	let sp = "[ \u{a0}]" in
	let p = Fmt.str "^%s*(.*?)%s*$" sp sp in
    let rex = Pcre.regexp ~flags:[`UTF8] p in
    Pcre.replace ~rex ~templ:"$1" x
end

module Option = struct
  let id x = x
  let all_none xs = match xs with
    | [] -> false
    | _ -> List.find_opt Option.is_some xs |> Option.is_none
  let join_some s xs = xs |> List.filter_map id |> RealString.concat s
end

module Rresult = struct
  let (let+) = fun a b -> Rresult.R.map b a
end

let mk_random_uuid =
  let open Uuidm in
  let rand = Random.State.make_self_init () in
  fun () -> to_string (v4_gen rand ())
