module Moves_Make (CpsLang : Lang.Cps.LANG) = struct
  type name = CpsLang.name
  type kind = CpsLang.name
  type data = CpsLang.nup
  type direction = Input | Output | None

  let string_of_direction = function
    | Input -> "?"
    | Output -> "!"
    | None -> "."

  let switch = function Input -> Output | Output -> Input | None -> None

  type move = direction * kind * data

  let string_of_move (direction, nn, nup) =
    CpsLang.string_of_name nn
    ^ string_of_direction direction
    ^ "(" ^ CpsLang.string_of_nup nup ^ ")"

  let get_data (_, _, d) = d
  let get_kind (_, k, _) = k
  let get_direction (p, _, _) = p
  let switch_direction (p, k, d) = (switch p, k, d)
  let get_transmitted_names (_, _, nup) = CpsLang.names_of_nup nup
  let get_subject_names (_, nn, _) = [ nn ]
end

module type INT = sig
  module ContNames : Lang.Cps.CONT_NAMES include Interactive.INT
end

module Int_Make (CpsLang : Lang.Cps.LANG) :
  INT
    with type OpLang.name = CpsLang.name
     and type ContNames.name = CpsLang.name
     and type Actions.Moves.name = CpsLang.name = struct
  module OpLang = CpsLang

  module ContNames = struct
    type name = CpsLang.name
    type cont_name = CpsLang.cont_name

    let inj_cont_name = CpsLang.inj_cont_name
    let get_cont_name = CpsLang.get_cont_name
    let string_of_cont_name = CpsLang.string_of_cont_name
  end

  module Actions = struct
    module Moves = Moves_Make (CpsLang)

    type action = PDiv | Vis of Moves.move

    let get_move_from_action = function Vis move -> Some move | PDiv -> None
    let inject_move move = Vis move
    let diverging_action = PDiv

    let string_of_action = function
      | PDiv -> "Div"
      | Vis move -> Moves.string_of_move move
  end

  open Actions

  let generate_output_action namectxO nn value =
    let ty_option = Util.Pmap.lookup nn namectxO in
    begin
      match ty_option with
      | Some ty ->
          let nty = CpsLang.neg_type ty in
          let (nup, ienv, namectxP) = CpsLang.abstract_ival value nty in
          (Vis (Moves.Output, nn, nup), ienv, namectxP)
      | None ->
          failwith
            ("Error: the name " ^ CpsLang.string_of_name nn
           ^ " is not in the name context "
            ^ CpsLang.string_of_name_type_ctx namectxO
            ^ ". Please report.")
    end

  let generate_input_moves namectxP =
    Util.Debug.print_debug "Generating O-moves";
    let aux (nn, ty) =
      if CpsLang.is_callable nn then
        let nups = CpsLang.generate_nup namectxP (CpsLang.neg_type ty) in
        List.map
          (fun (nup, namectx') -> ((Moves.Input, nn, nup), namectx'))
          nups
      else [] in
    List.flatten (Util.Pmap.map_list aux namectxP)

  let check_input_move namectxP namectxO (dir, name, nup) =
    match dir with
    | Moves.Output -> None
    | Moves.Input -> begin
        match Util.Pmap.lookup name namectxP with
        | None -> None
        | Some ty ->
            CpsLang.type_check_nup namectxP namectxO (CpsLang.neg_type ty) nup
      end
    | Moves.None -> None

  let trigger_computation ienv input_move =
    match input_move with
    | (Moves.Input, name, nup) -> begin
        match CpsLang.lookup_ienv name ienv with
        | Some value ->
            let nup' = CpsLang.subst_names_of_nup ienv nup in
            CpsLang.val_composition value nup'
        | None ->
            failwith
              ("Error: the move "
              ^ Moves.string_of_move input_move
              ^ " is ill-formed: the name "
              ^ CpsLang.string_of_name name
              ^ " is not in the environment "
              ^ CpsLang.string_of_ienv ienv
              ^ ". Please report.")
      end
    | _ ->
        failwith
          ("Error: the move "
          ^ Moves.string_of_move input_move
          ^ " is not an Opponent move. Please report.")

  let unify_move span move1 move2 =
    match (move1, move2) with
    | ((Moves.Output, nn1, nup1), (Moves.Output, nn2, nup2))
    | ((Moves.Input, nn1, nup1), (Moves.Input, nn2, nup2)) ->
        if Util.Namespan.is_in_dom_im (nn1, nn2) span then
          CpsLang.unify_nup span nup1 nup2
        else None
    | _ -> None

  let synch_move span move1 move2 =
    match (move1, move2) with
    | ((Moves.Output, nn1, nup1), (Moves.Input, nn2, nup2))
    | ((Moves.Input, nn1, nup1), (Moves.Output, nn2, nup2)) ->
        if Util.Namespan.is_in_dom_im (nn1, nn2) span then
          CpsLang.unify_nup span nup1 nup2
        else None
    | _ -> None

  let unify_action span act1 act2 =
    match (act1, act2) with
    | (Vis move1, Vis move2) -> unify_move span move1 move2
    | (PDiv, PDiv) -> Some span
    | (Vis _, PDiv) | (PDiv, Vis _) -> None
end
