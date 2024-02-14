(* --- usage: `xs $ n` for `n`th element of a list *)
let ($) = fun xs n -> List.nth xs n

module String = struct
  let strip_spaces s =
    let f = function
      | ' ' -> Seq.empty
      | x -> Seq.return x in
    String.to_seq s
    |> Seq.flat_map f
    |> String.of_seq
  let trim x =
	let sp = "[ \u{a0}]" in
	let p = Fmt.str "^%s*(.*?)%s*$" sp sp in
	let re = Re.Perl.compile_pat p in
	let f groups = Re.Group.get groups 1 in
	Re.replace re ~f x
end
