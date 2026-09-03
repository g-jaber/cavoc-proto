(* Single-play definability: synthesizing from an innocent view function a
   program of the concrete language, the lexically threaded pending
   continuation acting as the well-bracketing stack. *)

module Make
    (InteractiveOps : Lang.Definability.INTERACTIVE_OPS)
    (ViewFunction :
      Lts.View_function.VIEWFUNCTION
        with type TypingLTS.Moves.copattern =
          InteractiveOps.abstract_normal_form
          * InteractiveOps.Renaming.Namectx.t
         and module TypingLTS.Moves.Renaming = InteractiveOps.Renaming)
    (Reification :
      Reification.REIFICATION
        with type move = ViewFunction.TypingLTS.Moves.move
         and type name = InteractiveOps.Renaming.Namectx.Names.name
         and type pattern = ViewFunction.ExtraMemory.pattern
         and type state = ViewFunction.ExtraMemory.state
         and type oplang_store = InteractiveOps.store
         and type oplang_term = InteractiveOps.term
         and type oplang_pattern = InteractiveOps.source_pattern
         and type oplang_value = InteractiveOps.value) : sig
  (* The client program of a strategy opening with an initial move, its own Player move
     at the empty view. *)
  val synthesize_client_program :
    ViewFunction.t ->
    ViewFunction.ExtraMemory.state ->
    ViewFunction.TypingLTS.Moves.move ->
    ViewFunction.TypingLTS.position ->
    ( InteractiveOps.Renaming.Namectx.Names.name,
      InteractiveOps.value )
    Util.Pmap.pmap ->
    InteractiveOps.Renaming.Namectx.Names.name option ->
    InteractiveOps.term

  (* The value implementing each initial Player name, in context order; the
     private declarations are added by the module packaging, not here. *)
  val synthesize_module_program :
    ViewFunction.t ->
    ViewFunction.ExtraMemory.state ->
    ViewFunction.TypingLTS.position ->
    ( InteractiveOps.Renaming.Namectx.Names.name,
      InteractiveOps.value )
    Util.Pmap.pmap ->
    (InteractiveOps.Renaming.Namectx.Names.name * InteractiveOps.value) list

  (* The client of a module, from the play recorded against it, with its
     last Opponent move removed, and dualized. *)
  val synthesize_client_program_of_play :
    ?final_move:
      InteractiveOps.Renaming.Namectx.t * ViewFunction.TypingLTS.Moves.move ->
    ViewFunction.TypingLTS.position ->
    ViewFunction.TypingLTS.Moves.pol_move list ->
    (InteractiveOps.Renaming.Namectx.Names.name -> InteractiveOps.value) ->
    InteractiveOps.term option

  (* Dually, the module the same play is recorded against, with its last
     Player move removed, and the final memory state its private declarations
     are read from. Its Opponent context must be empty. *)
  val synthesize_module_program_of_play :
    ViewFunction.TypingLTS.position ->
    ViewFunction.TypingLTS.Moves.pol_move list ->
    ((InteractiveOps.Renaming.Namectx.Names.name * InteractiveOps.value) list
    * ViewFunction.ExtraMemory.state)
    option
end = struct
  module Moves = ViewFunction.TypingLTS.Moves
  module Renaming = InteractiveOps.Renaming
  module Namectx = Renaming.Namectx
  module Play = ViewFunction.Play

  (* The state at a program point, carrying the P-view with its two view-local
     contexts and the value of each non-continuation O-name. *)
  type definability_scope = {
    initial_namectxP: Namectx.t;
    view: ViewFunction.view_play;
    view_namectxP: Namectx.t;
    view_namectxO: Namectx.t;
    opponent_name_values:
      (Namectx.Names.name, InteractiveOps.value) Util.Pmap.pmap;
    pending_continuation: Namectx.Names.name option;
    memory: ViewFunction.ExtraMemory.state;
  }

  let subject_is_continuation move =
    Namectx.Names.is_cname (Moves.get_subject_name move)

  (* The weakening of the memory's final provided context after the view's
     Opponent names, as in a stored Player move. *)
  let provided_context_weakening scope =
    ViewFunction.TypingLTS.Moves.Renaming.weak_r
      (ViewFunction.ExtraMemory.provided_context scope.memory)
      scope.view_namectxO

  (* The provided levels a Player move reads: its subject when provided, the
     fragment leaving the subject as its only free name. *)
  let provided_levels_read scope player_move =
    match
      ViewFunction.TypingLTS.Moves.Renaming.lookup_inv
        (provided_context_weakening scope)
        (Moves.get_subject_name player_move)
    with
    | Some provided_level -> [ provided_level ]
    | None -> []

  (* The value of a Player move's free name: an in-view one through the
     environment, a provided one through the branch's read at its provided
     level. *)
  let value_of_opponent_name scope value_of_provided_level o_level =
    match Util.Pmap.lookup o_level scope.opponent_name_values with
    | Some value -> value
    | None -> begin
        match
          ViewFunction.TypingLTS.Moves.Renaming.lookup_inv
            (provided_context_weakening scope)
            o_level
        with
        | Some provided_level -> value_of_provided_level provided_level
        | None ->
            Util.Error.failwithf
              "Definability: the free name %a of a Player move has no value \
               and is not a provided level. Please report."
              Namectx.Names.pp_name o_level
      end

  (* The scope inside a branch, where both view-local contexts grow and the
     pattern's non-continuation names take the values of their binders. *)
  let extend_scope_through_op_move scope pattern player_move value_of_bound_name
      pending_continuation =
    let pattern_namectx = Moves.get_namectx pattern in
    let opponent_renaming =
      Renaming.weak_r pattern_namectx scope.view_namectxO in
    let extended_name_values =
      List.fold_left
        (fun name_values delta_name ->
          if Namectx.Names.is_cname delta_name then name_values
          else
            Util.Pmap.add
              ( Renaming.lookup opponent_renaming delta_name,
                value_of_bound_name delta_name )
              name_values)
        scope.opponent_name_values
        (Namectx.get_names pattern_namectx) in
    {
      view= scope.view @ [ { ViewFunction.o= pattern; p= player_move } ];
      view_namectxP=
        Namectx.concat scope.view_namectxP (Moves.get_namectx player_move);
      view_namectxO= Namectx.concat scope.view_namectxO pattern_namectx;
      opponent_name_values= extended_name_values;
      pending_continuation;
      memory= scope.memory;
      initial_namectxP= scope.initial_namectxP;
    }

  (* The view-local level a question pattern's fresh continuation gets. *)
  let question_pattern_continuation scope pattern =
    let pattern_namectx = Moves.get_namectx pattern in
    match
      List.filter Namectx.Names.is_cname (Namectx.get_names pattern_namectx)
    with
    | [ delta_continuation ] ->
        Renaming.lookup
          (Renaming.weak_r pattern_namectx scope.view_namectxO)
          delta_continuation
    | _ ->
        failwith
          "Definability: a question introduces exactly one continuation. \
           Please report."

  (* [fresh_names_weakening] reads the names the Player move introduces in the
     Player context of the view before it. *)
  let rec synthesize_player_move_term strategy scope fresh_names_weakening
      value_of_provided_level player_move =
    (* Extracted first: this is where non-fragment Player moves fail. *)
    let oplang_abstract_val =
      InteractiveOps.abstract_val_of_a_nf (fst player_move) in
    let value_of_free_name =
      value_of_opponent_name scope value_of_provided_level in
    let subject = Moves.get_subject_name player_move in
    if subject_is_continuation player_move then begin
      if scope.pending_continuation <> Some subject then
        Util.Error.failwithf
          "Definability: the Player move answers the continuation %a, which is \
           not the pending one; the strategy is not well-bracketed, so not \
           definable."
          Namectx.Names.pp_name subject;
      InteractiveOps.term_of_returned_value
        (synthesize_player_move_value strategy scope fresh_names_weakening
           value_of_free_name oplang_abstract_val)
    end
    else begin
      match
        List.filter Namectx.Names.is_cname
          (Moves.fresh_names fresh_names_weakening player_move)
      with
      | [ introduced_continuation ] ->
          InteractiveOps.pattern_matching_call
            (Reification.reify_state_reading scope.memory)
            ("result_" ^ Namectx.Names.string_of_name introduced_continuation)
            value_of_free_name subject
            (synthesize_player_move_value strategy scope fresh_names_weakening
               value_of_free_name oplang_abstract_val)
            (synthesize_return_branches strategy scope introduced_continuation)
      | _ ->
          failwith
            "Definability: a question introduces exactly one continuation. \
             Please report."
    end

  (* Free names come from the environment or the branch's reads, each fresh
     name from a recursively synthesized function at its assigned level. *)
  and synthesize_player_move_value strategy scope fresh_names_weakening
      value_of_free_name oplang_abstract_val =
    InteractiveOps.value_of_abstract_val value_of_free_name
      (fun delta_name ->
        synthesize_matching_function strategy scope
          (Renaming.lookup fresh_names_weakening delta_name))
      oplang_abstract_val

  (* One branch per P-view, its body wrapped in the memory's reads and
     advance, disagreeing P-views at a pattern each carrying their reified
     guard. *)
  (* Agreement compares the stored Player move whole and the occurrences'
     provided levels, so the reified advances of the occurrences merged into
     one branch agree too. *)
  and compile_guarded_branches strategy scope pending_continuation_at
      (pattern, guarded_player_moves) =
    let branch_body guard player_move value_of_bound_name =
      let fresh_names_weakening =
        ViewFunction.fresh_names_weakening scope.initial_namectxP scope.view
          player_move in
      Reification.reify_reads (provided_levels_read scope player_move)
        scope.memory (fun value_of_provided_level ->
          InteractiveOps.sequence
            (Reification.reify_advance pattern guard value_of_bound_name
               scope.memory)
            (synthesize_player_move_term strategy
               (extend_scope_through_op_move scope pattern player_move
                  value_of_bound_name
                  (pending_continuation_at pattern))
               fresh_names_weakening value_of_provided_level player_move)) in
    let oplang_abstract_val =
      InteractiveOps.abstract_val_of_a_nf (fst pattern) in
    (* A bound name is named after its view-local level. *)
    let opponent_renaming =
      Renaming.weak_r (Moves.get_namectx pattern) scope.view_namectxO in
    let identifier_of_bound_name delta_name =
      "opponent_"
      ^ Namectx.Names.string_of_name
          (Renaming.lookup opponent_renaming delta_name) in
    let branch guard reified_guard player_move : InteractiveOps.branch =
      {
        pattern= oplang_abstract_val;
        guard= reified_guard;
        identifier_of_bound_name;
        body= branch_body guard player_move;
      } in
    match Util.Pmap.to_list guarded_player_moves with
    | [] -> []
    | (guard, player_move) :: others
      when List.for_all
             (fun (guard', player_move') ->
               player_move' = player_move
               && ViewFunction.ExtraMemory.provided_levels_at guard'
                    scope.memory
                  = ViewFunction.ExtraMemory.provided_levels_at guard
                      scope.memory)
             others ->
        [ branch guard None player_move ]
    | _ ->
        Util.Pmap.map_list
          (fun (guard, player_move) ->
            branch guard (Some (Reification.reify_pattern guard)) player_move)
          guarded_player_moves

  (* Match the argument against the patterns recorded at the pointed view. *)
  and synthesize_matching_function strategy scope subject_level =
    let subject_type = Namectx.lookup_exn scope.view_namectxP subject_level in
    let compile_group ((pattern, _) as group) =
      if subject_is_continuation pattern then
        failwith
          "Definability: an answer pattern at a function name. Please report.";
      compile_guarded_branches strategy scope
        (fun pattern -> Some (question_pattern_continuation scope pattern))
        group in
    InteractiveOps.pattern_matching_abstraction
      (Reification.reify_state_reading scope.memory)
      ("argument_" ^ Namectx.Names.string_of_name subject_level)
      subject_type
      (List.concat
         (Util.Pmap.map_list compile_group
            (ViewFunction.guarded_branches_at strategy scope.view subject_level)))

  (* The returns at a call's fresh continuation; the pending continuation is
     unchanged through them, which is the bracketing discipline. *)
  and synthesize_return_branches strategy scope continuation_level =
    let compile_group ((pattern, _) as group) =
      if not (subject_is_continuation pattern) then
        failwith
          "Definability: a question pattern at a continuation name. Please \
           report.";
      compile_guarded_branches strategy scope
        (fun _ -> scope.pending_continuation)
        group in
    List.concat
      (Util.Pmap.map_list compile_group
         (ViewFunction.guarded_branches_at strategy scope.view
            continuation_level))

  let initial_scope position opponent_name_values pending_continuation
      view_namectxP memory =
    {
      initial_namectxP= view_namectxP;
      view= [];
      view_namectxP;
      view_namectxO=
        ViewFunction.opponent_context_of_view
          (ViewFunction.TypingLTS.get_namectxO position)
          [];
      opponent_name_values;
      pending_continuation;
      memory;
    }

  (* The initial move's fresh names get empty-view matching functions; the private store is
     allocated around the whole term. *)
  let synthesize_client_program strategy memory initial_move position
      opponent_name_values toplevel_continuation =
    if ViewFunction.TypingLTS.get_namectxP position <> Namectx.empty then
      failwith
        "Definability: a client strategy must start with an empty Player \
         context.";
    let localized_initial_move = Moves.erase_display_hints initial_move in
    let scope =
      initial_scope position opponent_name_values toplevel_continuation
        (ViewFunction.player_context_of_view
           (Moves.get_namectx localized_initial_move)
           [])
        memory in
    (* The initial move is emitted at the initial memory, before any Opponent move:
       it can use no provided name. *)
    (* Its names are the initial Player context, hence their own levels. *)
    InteractiveOps.allocate_store
      (Reification.reify_store_declarations memory)
      (synthesize_player_move_term strategy scope
         (Renaming.id (Moves.get_namectx localized_initial_move))
         (fun provided_level ->
           Util.Error.failwithf
             "Definability: the initial move reads the provided level %a. \
              Please report."
             Namectx.Names.pp_name provided_level)
         localized_initial_move)

  let synthesize_module_program strategy memory position opponent_name_values =
    let scope =
      initial_scope position opponent_name_values None
        (ViewFunction.player_context_of_view
           (ViewFunction.TypingLTS.get_namectxP position)
           [])
        memory in
    List.map
      (fun name -> (name, synthesize_matching_function strategy scope name))
      (Namectx.get_names scope.view_namectxP)

  (* Every question answered. *)
  let is_complete actions =
    List.fold_left
      (fun pending (_, move) ->
        if subject_is_continuation move then pending - 1 else pending + 1)
      0 actions
    = 0

  let synthesize_client_program_of_play ?final_move position actions
      value_of_imported_name =
    let module_exports = ViewFunction.TypingLTS.get_namectxP position in
    let final_move =
      Option.map
        (fun (continuation_namectx, move) ->
          ( continuation_namectx,
            Moves.map_free_names
              (Renaming.lookup
                 (Renaming.weak_r continuation_namectx module_exports))
              move ))
        final_move in
    let dual_position =
      ViewFunction.TypingLTS.init_act_pos
        (ViewFunction.TypingLTS.get_storectx position)
        (ViewFunction.TypingLTS.get_namectxO position)
        (match final_move with
        | None -> module_exports
        | Some (continuation_namectx, _) ->
            Namectx.concat module_exports continuation_namectx) in
    let dual path =
      match Play.dual dual_position path with
      | Some dual -> dual
      | None ->
          failwith
            "Definability: the play does not dualize into an environment play, \
             as when the module exports an abstract type, so it defines no \
             client." in
    let (Play.Any_end (path, ending)) =
      Play.path_of_actions Play.Passive position actions in
    let client_play : (Play.active, Play.passive) Play.path option =
      match ending with
      | Play.Passive -> begin
          let dual = dual path in
          match final_move with
          | Some (_, move) when actions <> [] && is_complete actions ->
              Some (Play.extend_by_player_move dual move)
          | _ -> Play.drop_last_move dual
        end
      | Play.Active -> Some (dual path) in
    match client_play with
    | None -> None
    | Some play ->
        let (initial_step, tail) = Play.player_step play in
        let initial_move = initial_step.Play.move in
        let (strategy, memory) = ViewFunction.add_play ViewFunction.empty tail in
        let opponent_name_values =
          Util.Pmap.list_to_pmap
            (List.map
               (fun nn -> (nn, value_of_imported_name nn))
               (Namectx.get_names module_exports)) in
        Some
          (synthesize_client_program strategy memory initial_move dual_position
             opponent_name_values
             (Option.map
                (fun (_, move) -> Moves.get_subject_name move)
                final_move))

  let synthesize_module_program_of_play position actions =
    let (Play.Any_end (path, ending)) =
      Play.path_of_actions Play.Passive position actions in
    let play : Play.t option =
      match ending with
      | Play.Passive -> Some path
      | Play.Active -> Play.drop_last_move path in
    match play with
    | None -> None
    | Some play when Option.is_none (Play.opponent_step play) -> None
    | Some play ->
        let (strategy, memory) = ViewFunction.add_play ViewFunction.empty play in
        Some
          ( synthesize_module_program strategy memory position Util.Pmap.empty,
            memory )
end
