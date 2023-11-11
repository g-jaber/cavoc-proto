module PogsLtsF (M : Util.Monad.BRANCH) (Int : Interactive.INT) = struct
  module M = M
  include M
  module Int = Int
  module Actions = Int.Actions

  type active_conf = {
    computation: Int.IntLang.Focusing.computation;
    heap: Int.IntLang.resources;
    loc_ctx: Int.IntLang.resources_type_ctx;
    namectxO: Int.IntLang.Focusing.name_type_ctx;
  }

  type passive_conf = {
    ienv: Int.IntLang.Focusing.interactive_env;
    loc_ctx: Int.IntLang.resources_type_ctx;
    namectxO: Int.IntLang.Focusing.name_type_ctx;
    namectxP: Int.IntLang.Focusing.name_type_ctx;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf aconf =
    "<"
    ^ Int.IntLang.Focusing.string_of_computation aconf.computation
    ^ " | "
    ^ Int.IntLang.string_of_resources aconf.heap
    ^ " | "
    ^ Int.IntLang.Focusing.string_of_name_type_ctx aconf.namectxO
    ^ ">"

  let string_of_passive_conf pconf =
    "<"
    ^ Int.IntLang.Focusing.string_of_interactive_env pconf.ienv
    ^ " | "
    ^ Int.IntLang.string_of_resources_type_ctx pconf.loc_ctx
    ^ " | "
    ^ Int.IntLang.Focusing.string_of_name_type_ctx pconf.namectxO
    ^ " | "
    ^ Int.IntLang.Focusing.string_of_name_type_ctx pconf.namectxP
    ^ ">"

  let p_trans aconf =
    let opconf_option = Int.IntLang.compute_nf (aconf.computation, aconf.heap) in
    match opconf_option with
    | None -> (Int.Actions.diverging_action, None)
    | Some (nf, heap) -> begin
        match Int.IntLang.Focusing.decompose_nf nf with
        | Some (nn, glue_val) ->
            let loc_ctx = Int.IntLang.resources_type_ctx_of_resources heap in
            let (move, ienv, namectxP) =
              Int.generate_output_action aconf.namectxO nn glue_val in
            (move, Some { loc_ctx; ienv; namectxP; namectxO= aconf.namectxO })
        | None -> (Int.Actions.error_action, None)
      end

  let o_trans pas_conf input_move =
    match
      Int.check_input_move pas_conf.namectxP pas_conf.namectxO input_move
    with
    | None -> None
    | Some _ ->
        failwith
          "POGS o_trans cannot be implemented without moves-with-heaps !!"

  let o_trans_gen pconf =
    let* (input_move, lnamectx) =
      M.para_list (Int.generate_input_moves pconf.namectxP) in
    let* heap = M.para_list (Int.IntLang.generate_resources pconf.loc_ctx) in
    let computation = Int.trigger_computation pconf.ienv input_move in
    return
      ( input_move,
        {
          computation;
          heap;
          loc_ctx= pconf.loc_ctx;
          namectxO= Util.Pmap.concat lnamectx pconf.namectxO;
        } )

  let init_aconf computation namectxO =
    {
      computation;
      heap= Int.IntLang.empty_resources;
      loc_ctx= Int.IntLang.empty_resources_type_ctx;
      namectxO;
    }

  let init_pconf resources ienv namectxO namectxP =
    let resouce_ctx = Int.IntLang.resources_type_ctx_of_resources resources in
    { loc_ctx= resouce_ctx; ienv; namectxO; namectxP }

  let equiv_aconf aconf aconfb =
    aconf.computation = aconfb.computation && aconf.heap = aconfb.heap
end
