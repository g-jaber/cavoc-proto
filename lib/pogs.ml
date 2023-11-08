module PogsLtsF (M : Util.Monad.BRANCH) (Int : Interactive.INT) = struct
  module M = M
  include M
  module Int = Int
  module Actions = Int.Actions

  type active_conf = {
    computation: Int.OpLang.computation;
    heap: Int.OpLang.resources;
    loc_ctx: Int.OpLang.resources_type_ctx;
    namectxO: Int.OpLang.name_type_ctx;
  }

  type passive_conf = {
    ienv: Int.OpLang.interactive_env;
    loc_ctx: Int.OpLang.resources_type_ctx;
    namectxO: Int.OpLang.name_type_ctx;
    namectxP: Int.OpLang.name_type_ctx;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let string_of_active_conf aconf =
    "<"
    ^ Int.OpLang.string_of_computation aconf.computation
    ^ " | "
    ^ Int.OpLang.string_of_resources aconf.heap
    ^ " | "
    ^ Int.OpLang.string_of_name_type_ctx aconf.namectxO
    ^ ">"

  let string_of_passive_conf pconf =
    "<"
    ^ Int.OpLang.string_of_ienv pconf.ienv
    ^ " | "
    ^ Int.OpLang.string_of_resources_type_ctx pconf.loc_ctx
    ^ " | "
    ^ Int.OpLang.string_of_name_type_ctx pconf.namectxO
    ^ " | "
    ^ Int.OpLang.string_of_name_type_ctx pconf.namectxP
    ^ ">"

  let p_trans aconf =
    let opconf_option = Int.OpLang.compute_nf (aconf.computation, aconf.heap) in
    match opconf_option with
    | None -> (Int.Actions.diverging_action, None)
    | Some ((_, heap) as opconf) ->
        let (nn, value) = Int.OpLang.decompose_nf opconf in
        let loc_ctx = Int.OpLang.resources_type_ctx_of_resources heap in
        let (move, ienv, namectxP) =
          Int.generate_output_action aconf.namectxO nn value in
        (move, Some { loc_ctx; ienv; namectxP; namectxO= aconf.namectxO })

  let o_trans pas_conf input_move =
    match Int.check_input_move pas_conf.namectxP input_move with
    | None -> None
    | Some _ ->
        failwith
          "POGS o_trans cannot be implemented without moves-with-heaps !!"

  let o_trans_gen pconf =
    let* (input_move, lnamectx) =
      M.para_list (Int.generate_input_moves pconf.namectxP) in
    let* heap = M.para_list (Int.OpLang.generate_resources pconf.loc_ctx) in
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
      heap= Int.OpLang.empty_resources;
      loc_ctx= Int.OpLang.empty_resources_type_ctx;
      namectxO;
    }

  let init_pconf resources ienv namectxO namectxP =
    let resouce_ctx = Int.OpLang.resources_type_ctx_of_resources resources in
    { loc_ctx= resouce_ctx; ienv; namectxO; namectxP }

  let equiv_aconf aconf aconfb =
    aconf.computation = aconfb.computation && aconf.heap = aconfb.heap
end
