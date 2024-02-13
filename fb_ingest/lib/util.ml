(* --- usage: `xs $ n` for `n`th element of a list *)
let ($) = fun xs n -> List.nth xs n

module String = struct
  let strip_spaces s =
    let f = function
      | ' ' -> Seq.empty
      | x -> String.to_seq (String.make 1 x) in
    String.to_seq s
    |> Seq.flat_map f
    |> String.of_seq

  let trim_front x =
    let b = Buffer.create 0 in
    let inside = ref false in
    let f c =
      let c' = if !inside then Some c else match c with
          (* --- @todo nicer way to write this? *)
          | x when x = BatUChar.chr 0x20 -> None
          | x when x = BatUChar.chr 0xa0 -> None
          | c ->
            let () = inside := true in
            Some c in
      match c' with
      | None -> ()
      | Some c'' -> Buffer.add_string b (BatUTF8.make 1 c'') in
    let () = BatUTF8.iter f x in
    Buffer.contents b
  (* --- not efficient, but works.
   * --- this trims more characters (e.g. weird unicode spaces) than String.trim *)
  let trim x = x
             |> trim_front
             |> BatString.rev
             |> trim_front
             |> BatString.rev
end
