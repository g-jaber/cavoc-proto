(* Focused transition tests for the thinning-backed visibility LTS. *)

module TestTypes = struct
  type t = string

  let to_yojson typ = `String typ
  let pp = Format.pp_print_string
end

module TestNames =
  Lang.Names.MakeInt
    (struct
      let is_callable = true
      let is_cname = false
    end)
    (struct
      let prefix = "n"
    end)
    ()

module TestNamectx = Lang.Typectx.Make_List (TestNames) (TestTypes)
module Weakening = Lang.Renaming.MakeWeak (TestNamectx)

module Moves :
  Lts.Moves.POLMOVES
    with module Renaming = Weakening
     and type copattern = Weakening.t = struct
  module Renaming = Weakening

  type copattern = Renaming.t
  type move = Renaming.Namectx.Names.name * copattern

  let get_subject_name = fst
  let get_namectx (_, placement) = Renaming.dom placement

  let get_fresh_names (_, placement) =
    List.map
      (Renaming.lookup placement)
      (Renaming.Namectx.get_names (Renaming.dom placement))

  let pp_move_in ~show_name fmt (subject, placement) =
    Format.fprintf fmt "%s{%a}" (show_name subject) Renaming.pp placement

  let pp_move =
    pp_move_in ~show_name:Renaming.Namectx.Names.string_of_name

  let string_of_move_in ~show_name =
    Format.asprintf "%a" (pp_move_in ~show_name)

  let string_of_move = Format.asprintf "%a" pp_move

  let move_to_yojson_in ~show_name move =
    `String (string_of_move_in ~show_name move)

  let move_to_yojson =
    move_to_yojson_in ~show_name:Renaming.Namectx.Names.string_of_name

  let unify_move span move1 move2 =
    if move1 = move2 then Some span else None

  type direction = Input | Output
  type pol_move = direction * move

  let string_of_direction = function Input -> "?" | Output -> "!"

  let pp_pol_move_in ~show_name fmt (direction, move) =
    Format.fprintf fmt "%s%a" (string_of_direction direction)
      (pp_move_in ~show_name) move

  let pp_pol_move =
    pp_pol_move_in ~show_name:Renaming.Namectx.Names.string_of_name

  let string_of_pol_move_in ~show_name =
    Format.asprintf "%a" (pp_pol_move_in ~show_name)

  let string_of_pol_move = Format.asprintf "%a" pp_pol_move

  let pol_move_to_yojson_in ~show_name move =
    `String (string_of_pol_move_in ~show_name move)

  let pol_move_to_yojson =
    pol_move_to_yojson_in ~show_name:Renaming.Namectx.Names.string_of_name

  let yojson_of_move move = `String (string_of_move move)

  let switch_direction (direction, move) =
    let direction' = match direction with Input -> Output | Output -> Input in
    (direction', move)

  let unify_pol_move span (direction1, move1) (direction2, move2) =
    if direction1 = direction2 then unify_move span move1 move2 else None
end

module TypingLTS :
  Lts.Typing.LTS
    with module Moves = Moves
     and type store_ctx = unit = struct
  module Moves = Moves
  module BranchMonad = Util.Monad.ListB

  type store_ctx = unit
  type status = Active | Passive

  type position = {
    status: status;
    namectxP: TestNamectx.t;
    namectxO: TestNamectx.t;
  }

  let position_to_yojson position =
    `Assoc
      [
        ("namectxP", TestNamectx.to_yojson position.namectxP);
        ("namectxO", TestNamectx.to_yojson position.namectxO);
      ]

  let pp_position fmt position =
    Format.fprintf fmt "P: %a; O: %a" TestNamectx.pp position.namectxP
      TestNamectx.pp position.namectxO

  let string_of_position = Format.asprintf "%a" pp_position
  let get_namectxP position = position.namectxP
  let get_namectxO position = position.namectxO
  let get_storectx _ = ()

  let init_act_pos () namectxP namectxO =
    { status= Active; namectxP; namectxO }

  let init_pas_pos () namectxP namectxO =
    { status= Passive; namectxP; namectxO }

  let place position direction _subject local_namectx =
    match direction with
    | Moves.Output -> Weakening.weak_r local_namectx position.namectxP
    | Moves.Input -> Weakening.weak_r local_namectx position.namectxO

  let check_move position ((direction, (_, placement)) : Moves.pol_move) =
    match (position.status, direction) with
    | (Active, Moves.Output) ->
        Some
          {
            status= Passive;
            namectxP= Weakening.im placement;
            namectxO= position.namectxO;
          }
    | (Passive, Moves.Input) ->
        Some
          {
            status= Active;
            namectxP= position.namectxP;
            namectxO= Weakening.im placement;
          }
    | _ -> None

  let trigger_move position move =
    match check_move position move with
    | Some position -> position
    | None -> failwith "wrong-polarity mock move"

  let generate_moves _ = BranchMonad.fail ()
end

module Vis = Ogs.Vis_lts.MakeNameIndexed (TypingLTS)

let failures = ref 0

let check name condition =
  if not condition then begin
    incr failures;
    Printf.eprintf "FAILED: %s\n" name
  end

let make_ctx entries =
  List.fold_left
    (fun ctx (hint, typ) -> snd (TestNamectx.add_fresh ctx hint typ))
    TestNamectx.empty entries

let move ~subject ~before ~fresh =
  (subject, Weakening.weak_r fresh before)

let some name = function
  | Some value -> value
  | None ->
      incr failures;
      Printf.eprintf "FAILED: %s\n" name;
      failwith name

let () =
  (* Initially o0 sees p0 and p1. *)
  let p0 = make_ctx [ ("p0", "A"); ("p1", "B") ] in
  let o0 = make_ctx [ ("o0", "X") ] in
  let active0 = Vis.init_act_pos () p0 o0 in

  (* Calling o0 introduces p2, so the current view becomes {p0,p1,p2}. *)
  let fresh_p2 = make_ctx [ ("p2", "C") ] in
  let p1 = TestNamectx.concat p0 fresh_p2 in
  let output_o0 = move ~subject:0 ~before:p0 ~fresh:fresh_p2 in
  let passive1 =
    some "the initial Player output is accepted"
      (Vis.check_move active0 (Moves.Output, output_o0))
  in

  (* Opponent calls p0 and introduces o1.  The map for o1 snapshots the
     current view, which contains p2. *)
  let fresh_o1 = make_ctx [ ("o1", "Y") ] in
  let o1 = TestNamectx.concat o0 fresh_o1 in
  let input_p0 = move ~subject:0 ~before:o0 ~fresh:fresh_o1 in
  let active1 =
    some "a visible Opponent input is accepted"
      (Vis.check_move passive1 (Moves.Input, input_p0))
  in

  let fresh_p3 = make_ctx [ ("p3", "D") ] in

  (* Calling the old name o0 restores its initial view, then adds p3.  The
     resulting sparse view is {p0,p1,p3}, so p2 is no longer visible. *)
  let output_old_o0 = move ~subject:0 ~before:p1 ~fresh:fresh_p3 in
  let passive_old =
    some "an output on the old Opponent name is accepted"
      (Vis.check_move active1 (Moves.Output, output_old_o0))
  in
  let no_fresh = TestNamectx.empty in
  let input_p2 = move ~subject:2 ~before:o1 ~fresh:no_fresh in
  let input_p3 = move ~subject:3 ~before:o1 ~fresh:no_fresh in
  check "restoring the older view hides the intervening Player name"
    (Vis.check_move passive_old (Moves.Input, input_p2) = None);
  check "the Player name introduced by the latest output is visible"
    (Option.is_some (Vis.check_move passive_old (Moves.Input, input_p3)));

  (* Calling o1 restores the later snapshot, which did contain p2. *)
  let output_new_o1 = move ~subject:1 ~before:p1 ~fresh:fresh_p3 in
  let passive_new =
    some "an output on the newer Opponent name is accepted"
      (Vis.check_move active1 (Moves.Output, output_new_o1))
  in
  check "restoring the newer view retains the intervening Player name"
    (Option.is_some (Vis.check_move passive_new (Moves.Input, input_p2)));

  check "an Opponent move cannot be played from an active history state"
    (Vis.check_move active1 (Moves.Input, input_p0) = None);
  check "a Player move cannot be played from a passive history state"
    (Vis.check_move passive_old (Moves.Output, output_old_o0) = None);

  if !failures = 0 then print_endline "vis_lts: all tests passed"
  else begin
    Printf.eprintf "vis_lts: %d test(s) failed\n" !failures;
    exit 1
  end
