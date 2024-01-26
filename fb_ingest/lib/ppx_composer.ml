open Ppxlib

(** Labels don't make sense with infix operators and these functions all assume Nolabel *)

let config, ops =
  let c = [
    "%>", `Forward;
    "%", `Backward;
  ]
  in c, List.map fst c

module U = struct
  let list_contains x = List.exists ((=) x)
end

module UE = struct
  let expr_is_apply e = match e.pexp_desc with
    | Pexp_apply _ -> true
    | _ -> false

  let expr_try_get_apply_ident_info e = match e.pexp_desc with
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident l; _ }; _ }, xs) ->
        let args = List.map snd xs in
        Some (l, args)
    | _ -> None

  let expr_try_get_apply_ident_info_with_2_args_and_id_in ids e =
    match expr_try_get_apply_ident_info e with
    | None -> None
    | Some (id, [_; _]) as i when U.list_contains id ids -> i
    | _ -> None
end

let apply ~loc f xs =
  let apply' = Ast_helper.Exp.apply ~loc in
  match f.pexp_desc with
   | Pexp_apply (f', xs') -> apply' f' (List.append xs' xs)
   | _ -> apply' f xs

(* todo avoid clashes *)
let fresh_var_for e =
  Printf.sprintf "___'''ppx_compose_%d'''___" e.pexp_loc.Location.loc_start.Lexing.pos_cnum

let reduce_compose compose_type =
  let rec reduce_compose' h x =
    match UE.expr_try_get_apply_ident_info_with_2_args_and_id_in ops h with
    | Some (_, [f; g]) ->
      let f', g' = begin match compose_type with
       | `Forward -> f, g
       | `Backward -> g, f end in
      let fx = reduce_compose' f' x in
      reduce_compose' g' fx
    | _ -> apply ~loc:h.pexp_loc h [Nolabel, x]
  in reduce_compose'

let eta_expand_composition compose_type e =
  let open Ast_helper in
  let name = fresh_var_for e in
  let var_loc = match compose_type with
    | `Forward  -> {e.pexp_loc with loc_end = e.pexp_loc.loc_start}
    | `Backward -> {e.pexp_loc with loc_start = e.pexp_loc.loc_end} in
  let pat = Pat.var ~loc:var_loc {txt = name; loc = var_loc} in
  let arg = Exp.ident ~loc:var_loc {txt = Lident name; loc = var_loc} in
  let body = reduce_compose compose_type e arg in
  Exp.fun_ ~loc:e.pexp_loc Nolabel None pat body

let rewrite compose_type e = match UE.expr_is_apply e with
 (* e.g. let c = (%>), not allowed, pass through and let compiler complain *)
 | false -> None
 (* e.g
  *   f %> g
  *   f 1 2 3 %> g 4 5 6
  *)
 | true -> Some (eta_expand_composition compose_type e)

let rules =
  let f (op, type') = Context_free.Rule.special_function op (rewrite type')
  in List.map f config

let () = Ppxlib.Driver.register_transformation ~rules "ppx_composer"
