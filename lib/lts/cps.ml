module Moves_Make (IntLang : Lang.Interactive.LANG) = struct
  type name = IntLang.name
  type kind = IntLang.name
  type data = IntLang.abstract_val
  type direction = Input | Output | None

  let string_of_direction = function
    | Input -> "?"
    | Output -> "!"
    | None -> "."

  let switch = function Input -> Output | Output -> Input | None -> None

  type move = direction * kind * data

  let string_of_move (direction, nn, aval) =
    IntLang.string_of_name nn
    ^ string_of_direction direction
    ^ IntLang.string_of_abstract_val aval

  let get_data (_, _, d) = d
  let get_kind (_, k, _) = k
  let get_direction (p, _, _) = p
  let switch_direction (p, k, d) = (switch p, k, d)
  let get_transmitted_names (_, _, aval) = IntLang.names_of_abstract_val aval
  let get_subject_names (_, nn, _) = [ nn ]
end

module type INT = sig
  module ContNames : Lang.Names.CONT_NAMES include Interactive.INT
end

module Int_Make (CpsLang : Lang.Cps.INTLANG) :
  INT
    with type IntLang.name = CpsLang.name
     and type ContNames.name = CpsLang.name
     and type Actions.Moves.name = CpsLang.name = struct
  module IntLang = CpsLang

  module ContNames = struct
    type name = CpsLang.name
    type cont_name = CpsLang.cont_name

    let string_of_name = CpsLang.string_of_name
    let is_callable = CpsLang.is_callable
    let inj_cont_name = CpsLang.inj_cont_name
    let get_cont_name = CpsLang.get_cont_name
    let string_of_cont_name = CpsLang.string_of_cont_name
    let fresh_cname = CpsLang.fresh_cname
  end

  module Actions = struct
    module Moves = Moves_Make (CpsLang)

    type action = PDiv | PError | Vis of Moves.move

    let get_move_from_action = function
      | Vis move -> Some move
      | PError | PDiv -> None

    let inject_move move = Vis move
    let diverging_action = PDiv
    let error_action = PError

    let string_of_action = function
      | PDiv -> "Div"
      | PError -> "Error"
      | Vis move -> Moves.string_of_move move
  end

  open Actions

  let generate_output_action namectxO nn glue_val =
    let ty_option = Util.Pmap.lookup nn namectxO in
    begin
      match ty_option with
      | Some ty ->
          let nty = CpsLang.neg_type ty in
          let (aval, ienv, namectxP) = CpsLang.abstract_glue_val glue_val nty in
          (Vis (Moves.Output, nn, aval), ienv, namectxP)
      | None ->
          failwith
            ("Error: the name " ^ CpsLang.string_of_name nn
           ^ " is not in the name context "
           (*^ CpsLang.string_of_name_type_ctx namectxO*)
           ^ ". Please report.")
    end

  open IntLang.M

  let generate_input_moves namectxP =
    Util.Debug.print_debug "Generating O-moves";
    let namectxP' = Util.Pmap.filter_dom CpsLang.is_callable namectxP in
    let* (nn, ty) = IntLang.M.para_list @@ Util.Pmap.to_list namectxP' in
    let* (aval, namectx) =
      CpsLang.generate_abstract_val namectxP (CpsLang.neg_type ty) in
    return ((Moves.Input, nn, aval), namectx)

  let check_input_move namectxP namectxO (dir, name, aval) =
    match dir with
    | Moves.Output -> None
    | Moves.Input -> begin
        match Util.Pmap.lookup name namectxP with
        | None -> None
        | Some ty ->
            CpsLang.type_check_abstract_val namectxP namectxO
              (CpsLang.neg_type ty) aval
      end
    | Moves.None -> None

  let trigger_computation ienv input_move =
    match input_move with
    | (Moves.Input, name, aval) -> begin
        match CpsLang.lookup_ienv name ienv with
        | Some value -> CpsLang.val_composition ienv value aval
        | None ->
            failwith
              ("Error: the move "
              ^ Moves.string_of_move input_move
              ^ " is ill-formed: the name "
              ^ CpsLang.string_of_name name
              ^ " is not in the environment "
              ^ CpsLang.string_of_interactive_env ienv
              ^ ". Please report.")
      end
    | _ ->
        failwith
          ("Error: the move "
          ^ Moves.string_of_move input_move
          ^ " is not an Opponent move. Please report.")

  let unify_move span move1 move2 =
    match (move1, move2) with
    | ((Moves.Output, nn1, aval1), (Moves.Output, nn2, aval2))
    | ((Moves.Input, nn1, aval1), (Moves.Input, nn2, aval2)) ->
        if Util.Namespan.is_in_dom_im (nn1, nn2) span then
          CpsLang.unify_abstract_val span aval1 aval2
        else None
    | _ -> None

  let synch_move span move1 move2 =
    match (move1, move2) with
    | ((Moves.Output, nn1, aval1), (Moves.Input, nn2, aval2))
    | ((Moves.Input, nn1, aval1), (Moves.Output, nn2, aval2)) ->
        if Util.Namespan.is_in_dom_im (nn1, nn2) span then
          CpsLang.unify_abstract_val span aval1 aval2
        else None
    | _ -> None

  let unify_action span act1 act2 =
    match (act1, act2) with
    | (Vis move1, Vis move2) -> unify_move span move1 move2
    | (PDiv, PDiv) -> Some span
    | (PError, PError) -> Some span
    | _ -> None
end
