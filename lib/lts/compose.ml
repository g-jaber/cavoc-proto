(* This module provides two functions to compose the interaction between
    two configurations of the same bipartite LTS.
   One of the configuration must be active, and the other one is passive.
*)

module Make =
functor
  (IntLTS : Strategy.LTS)
  ->
  struct
    include Util.Monad.UserChooseWrite (struct
      type t = IntLTS.TypingLTS.Moves.pol_move

      let show = IntLTS.TypingLTS.Moves.string_of_pol_move
    end)

    type player = Proponent | Opponent

    (* compose_check first retrieves the action performed by the active configuration,
       then it checks if the dual action is a valid one for the passive configuration.*)
    let rec compose_check confP confO =
      Util.Debug.print_debug "One step of composition";
      let (act_conf, pas_conf, act_player) =
        match (confP, confO) with
        | (IntLTS.Active aconf, IntLTS.Passive pconf) ->
            (aconf, pconf, Proponent)
        | (IntLTS.Passive pconf, IntLTS.Active aconf) -> (aconf, pconf, Opponent)
        | (IntLTS.Active _, IntLTS.Active _) ->
            failwith
              "Error: trying to compose two active configurations. Please \
               report."
        | (IntLTS.Passive _, IntLTS.Passive _) ->
            failwith
              "Error: trying to compose two passive configurations. Please \
               report." in
      match IntLTS.EvalMonad.run (IntLTS.p_trans act_conf) with
      | None ->
          Util.Debug.print_debug "Stopping composition";
          return ()
      | Some (output_move, pas_conf') ->
          let input_move = IntLTS.TypingLTS.Moves.switch_direction output_move in
          begin
            match IntLTS.o_trans pas_conf input_move with
            | None ->
                Util.Debug.print_debug
                  ("Input move forbidden: "
                  ^ IntLTS.TypingLTS.Moves.string_of_pol_move input_move
                  ^ " in the configuration "
                  ^ IntLTS.string_of_passive_conf pas_conf);
                emit output_move
            | Some act_conf' ->
                let (moveP, confP', confO') =
                  begin
                    match act_player with
                    | Proponent ->
                        ( output_move,
                          IntLTS.Passive pas_conf',
                          IntLTS.Active act_conf' )
                    | Opponent ->
                        ( input_move,
                          IntLTS.Active act_conf',
                          IntLTS.Passive pas_conf' )
                  end in
                Util.Debug.print_debug @@ "Composition succeeded with move "
                ^ IntLTS.TypingLTS.Moves.string_of_pol_move moveP;
                let* () = emit moveP in
                compose_check confP' confO'
          end

    (* compose_gen retrieves the action performed by the active configuration,
       then generates all the moves performed by the passive configurations,
       trying to compose them.
       It uses a span of names to compose the two actions. *)
    let rec compose_gen nspan confP confO =
      Util.Debug.print_debug "One step of composition";
      let (act_conf, pas_conf, act_player) =
        match (confP, confO) with
        | (IntLTS.Active aconf, IntLTS.Passive pconf) ->
            (aconf, pconf, Proponent)
        | (IntLTS.Passive pconf, IntLTS.Active aconf) -> (aconf, pconf, Opponent)
        | (IntLTS.Active _, IntLTS.Active _) ->
            failwith
              "Error: trying to compose two active configurations. Please \
               report."
        | (IntLTS.Passive _, IntLTS.Passive _) ->
            failwith
              "Error: trying to compose two passive configurations. Please \
               report." in
      match IntLTS.EvalMonad.run (IntLTS.p_trans act_conf) with
      | None ->
          Util.Debug.print_debug "Stopping composition";
          return ()
      | Some (output_move, pas_conf') ->
          let* (input_move, act_conf') =
            para_list (IntLTS.TypingLTS.BranchMonad.run (IntLTS.o_trans_gen pas_conf))
          in
          let (moveP, moveO, confP', confO') =
            begin
              match act_player with
              | Proponent ->
                  ( output_move,
                    input_move,
                    IntLTS.Passive pas_conf',
                    IntLTS.Active act_conf' )
              | Opponent ->
                  ( input_move,
                    output_move,
                    IntLTS.Active act_conf',
                    IntLTS.Passive pas_conf' )
            end in
          Util.Debug.print_debug
            ("Composing moves "
            ^ IntLTS.TypingLTS.Moves.string_of_pol_move moveP
            ^ " and "
            ^ IntLTS.TypingLTS.Moves.string_of_pol_move moveO);
          let moveO' = IntLTS.TypingLTS.Moves.switch_direction moveO in
          let nspan_option = IntLTS.TypingLTS.Moves.unify_pol_move nspan moveP moveO' in
          begin
            match nspan_option with
            | None ->
                let span_string =
                  Util.Namespan.string_of_span
                    IntLTS.TypingLTS.Moves.Renaming.Namectx.Names.string_of_name nspan in
                Util.Debug.print_debug
                  ("Composing failed in namespan " ^ span_string);
                return ()
            | Some nspan' ->
                Util.Debug.print_debug @@ "Composing succeeded with move "
                ^ IntLTS.TypingLTS.Moves.string_of_pol_move moveP;
                let* () = emit moveP in
                compose_gen nspan' confP' confO'
          end

    let get_traces_check act_conf pas_conf =
      let result = compose_check act_conf pas_conf in
      let trace_list = get_trace result in
      List.map string_of_trace trace_list

    let get_traces_gen namespan act_conf pas_conf =
      let result = compose_gen namespan act_conf pas_conf in
      let trace_list = get_trace result in
      List.map string_of_trace trace_list
  end
