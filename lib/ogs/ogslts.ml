module Make (Int : Lts.Interactive.INT) = struct
  module M = Int.IntLang.M
  include M
  module Int = Int
  module Actions = Int.Actions

  type active_conf = {
    computation: Int.IntLang.computation;
    heap: Int.IntLang.Memory.memory;
    ienv: Int.IntLang.interactive_env;
    namectxO: Int.IntLang.name_type_ctx;
    namectxP: Int.IntLang.name_type_ctx;
  }

  type passive_conf = {
    heap: Int.IntLang.Memory.memory;
    ienv: Int.IntLang.interactive_env;
    namectxO: Int.IntLang.name_type_ctx;
    namectxP: Int.IntLang.name_type_ctx;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf act_conf =
    "<"
    ^ Int.IntLang.string_of_computation act_conf.computation
    ^ " | "
    ^ Int.IntLang.Memory.string_of_memory act_conf.heap
    ^ " | "
    ^ Int.IntLang.string_of_interactive_env act_conf.ienv
    ^ " | "
    ^ Int.IntLang.string_of_name_type_ctx act_conf.namectxO
    ^ " | "
    ^ Int.IntLang.string_of_name_type_ctx act_conf.namectxP
    ^ ">"

  let string_of_passive_conf pas_conf =
    "<"
    ^ Int.IntLang.Memory.string_of_memory pas_conf.heap
    ^ " | "
    ^ Int.IntLang.string_of_interactive_env pas_conf.ienv
    ^ " | "
    ^ Int.IntLang.string_of_name_type_ctx pas_conf.namectxO
    ^ " | "
    ^ Int.IntLang.string_of_name_type_ctx pas_conf.namectxP
    ^ ">"

  let p_trans act_conf =
    let nf_option =
      Int.IntLang.compute_nf (act_conf.computation, act_conf.heap) in
    match nf_option with
    | None -> (Int.Actions.diverging_action, None)
    | Some (None,_) -> (Int.Actions.error_action, None)
    | Some (Some kind, heap) ->
        let (move, ienv', namectxP') =
          Int.generate_output_move act_conf.namectxO kind in
        ( Int.Actions.inject_move move,
          Some
            {
              heap;
              ienv= Int.IntLang.concat_ienv ienv' act_conf.ienv;
              namectxP=
                Int.IntLang.concat_name_type_ctx namectxP' act_conf.namectxP;
              namectxO= act_conf.namectxO;
            } )

  let o_trans pas_conf input_move =
    match
      Int.check_input_move pas_conf.namectxP pas_conf.namectxO input_move
    with
    | None -> None
    | Some (_, namectxO) ->
        let (computation, ienv) =
          Int.trigger_computation pas_conf.ienv input_move in
        Some
          {
            computation;
            heap= pas_conf.heap;
            ienv;
            namectxO;
            namectxP= pas_conf.namectxP;
          }

  let o_trans_gen pas_conf =
    let* (input_move, _, namectxO) =
      Int.generate_input_moves pas_conf.namectxP pas_conf.namectxO in
    let (computation, ienv) = Int.trigger_computation pas_conf.ienv input_move in
    return
      ( input_move,
        {
          computation;
          heap= pas_conf.heap;
          ienv;
          namectxO;
          namectxP= pas_conf.namectxP;
        } )

  let init_aconf computation namectxO =
    {
      computation;
      heap= Int.IntLang.Memory.empty_memory;
      ienv= Int.IntLang.empty_ienv;
      namectxO;
      namectxP= Int.IntLang.empty_name_type_ctx;
    }

  let init_pconf memory ienv namectxP namectxO =
    { heap= memory; ienv; namectxO; namectxP }

  let equiv_aconf act_conf aconfb =
    act_conf.computation = aconfb.computation && act_conf.heap = aconfb.heap
end
