module OgsLtsF =
functor
  (M : Util.Monad.BRANCH)
  (Int : Interactive.INT)
  ->
  struct
    module M = M
    include M
    module Int = Int
    module Actions = Int.Actions

    type active_conf = {
      computation: Int.OpLang.computation;
      heap: Int.OpLang.resources;
      ienv: Int.OpLang.interactive_env;
      namectxO: Int.OpLang.name_type_ctx;
      namectxP: Int.OpLang.name_type_ctx;
    }

    type passive_conf = {
      heap: Int.OpLang.resources;
      ienv: Int.OpLang.interactive_env;
      namectxO: Int.OpLang.name_type_ctx;
      namectxP: Int.OpLang.name_type_ctx;
    }

    type conf = Active of active_conf | Passive of passive_conf

    let string_of_active_conf act_conf =
      "<"
      ^ Int.OpLang.string_of_computation act_conf.computation
      ^ " | "
      ^ Int.OpLang.string_of_resources act_conf.heap
      ^ " | "
      ^ Int.OpLang.string_of_ienv act_conf.ienv
      ^ " | "
      ^ Int.OpLang.string_of_name_type_ctx act_conf.namectxO
      ^ " | "
      ^ Int.OpLang.string_of_name_type_ctx act_conf.namectxP
      ^ ">"

    let string_of_passive_conf pas_conf =
      "<"
      ^ Int.OpLang.string_of_resources pas_conf.heap
      ^ " | "
      ^ Int.OpLang.string_of_ienv pas_conf.ienv
      ^ " | "
      ^ Int.OpLang.string_of_name_type_ctx pas_conf.namectxO
      ^ " | "
      ^ Int.OpLang.string_of_name_type_ctx pas_conf.namectxP
      ^ ">"

    let p_trans act_conf =
      let opconf_option =
        Int.OpLang.compute_nf (act_conf.computation, act_conf.heap) in
      match opconf_option with
      | None -> (Int.Actions.diverging_action, None)
      | Some ((_, heap) as opconf) ->
          let (nn, value) = Int.OpLang.decompose_nf opconf in
          let (move, ienv', namectxP') =
            Int.generate_output_action act_conf.namectxO nn value in
          ( move,
            Some
              {
                heap;
                ienv= Int.OpLang.concat_ienv ienv' act_conf.ienv;
                namectxP= Util.Pmap.concat namectxP' act_conf.namectxP;
                namectxO= act_conf.namectxO;
              } )

    let o_trans pas_conf input_move =
      match Int.check_input_move pas_conf.namectxP input_move with
      | None -> None
      | Some lnamectx ->
          let computation = Int.trigger_computation pas_conf.ienv input_move in
          Some
            {
              computation;
              heap= pas_conf.heap;
              ienv= pas_conf.ienv;
              namectxO= Util.Pmap.concat lnamectx pas_conf.namectxO;
              namectxP= pas_conf.namectxP;
            }

    let o_trans_gen pas_conf =
      let* (input_move, lnamectx) =
        M.para_list (Int.generate_input_moves pas_conf.namectxP) in
      let computation = Int.trigger_computation pas_conf.ienv input_move in
      return
        ( input_move,
          {
            computation;
            heap= pas_conf.heap;
            ienv= pas_conf.ienv;
            namectxO= Util.Pmap.concat lnamectx pas_conf.namectxO;
            namectxP= pas_conf.namectxP;
          } )

    let init_aconf computation namectxO =
      {
        computation;
        heap= Int.OpLang.empty_resources;
        ienv= Int.OpLang.empty_ienv;
        namectxO;
        namectxP= Util.Pmap.empty;
      }

    let init_pconf resources ienv namectxP namectxO =
      { heap= resources; ienv; namectxO; namectxP }

    let equiv_aconf act_conf aconfb =
      act_conf.computation = aconfb.computation && act_conf.heap = aconfb.heap
  end
