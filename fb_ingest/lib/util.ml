let strip_spaces s =
  let f = function
    | ' ' -> Seq.empty
    | x -> String.to_seq (String.make 1 x) in
  String.to_seq s
  |> Seq.flat_map f
  |> String.of_seq
(* --- usage: `xs $ n` for `n`th element of a list *)
let ($) = fun xs n -> List.nth xs n
