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

  module Actions = Actions.Make (CpsLang)
  open Actions

  let generate_output_move namectxO kind glue_val =
    let ty_option = CpsLang.kind_interact_typing kind namectxO in
    begin
      match ty_option with
      | Some ty ->
          let nty = CpsLang.neg_type ty in
          let (aval, ienv, namectxP) = CpsLang.abstract_glue_val glue_val nty in
          (Moves.build (Moves.Output, kind, aval), ienv, namectxP)
      | None ->
          failwith
            ("Error: the interaction kind "
            ^ CpsLang.string_of_kind_interact kind
            ^ " is not typeable in the name context "
            ^ CpsLang.string_of_name_type_ctx namectxO
            ^ ". Please report.")
    end

  open IntLang.M

  let generate_input_moves namectxP =
    Util.Debug.print_debug "Generating O-moves";
    let aux (nn, ty) =
      match CpsLang.name_to_kind_interact nn with
      | None -> None
      | Some kind -> Some (kind, ty) in
    let kind_list = Util.Pmap.to_list @@ Util.Pmap.filter_map aux namectxP in
    let* (kind, ty) = IntLang.M.para_list @@ kind_list in
    let* (aval, namectx) =
      CpsLang.generate_abstract_val namectxP (CpsLang.neg_type ty) in
    return (Moves.build (Moves.Input, kind, aval), namectx)

  let check_input_move namectxP namectxO move =
    match Moves.get_direction move with
    | Moves.Output -> None
    | Moves.Input -> begin
        let kind = Moves.get_kind move in
        match CpsLang.kind_interact_typing kind namectxP with
        | None -> None
        | Some ty ->
            let aval = Moves.get_data move in
            CpsLang.type_check_abstract_val namectxP namectxO
              (CpsLang.neg_type ty) aval
      end
    | Moves.None -> None

  let trigger_computation ienv input_move =
    let nn_list = Moves.get_subject_names input_move in
    let aval = Moves.get_data input_move in
    match (Moves.get_direction input_move, nn_list) with
    | (Moves.Input, [ name ]) -> begin
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
end
