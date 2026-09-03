(* Strategies as view functions: partial maps from (P-view, Opponent move,
   extra argument) to Player move. *)

module type VIEWFUNCTION = sig
  module TypingLTS : Typing.LTS
  module Play : module type of Play.Make (TypingLTS)
  module View : module type of View.Make (TypingLTS.Moves.Renaming)

  (* The summary of the interaction beyond the P-view that Player moves may
     depend on, a P-view's guard being the complete read it performs on
     it. *)
  (* Two P-views that disagree at one guard measure the insufficiency of the
     chosen memory. *)
  module ExtraMemory :
    Extra_memory.EXTRA_MEMORY
      with type move = TypingLTS.Moves.move
       and type name = TypingLTS.Moves.Renaming.Namectx.Names.name
       and type namectx = TypingLTS.Moves.Renaming.Namectx.t
       and type renaming = TypingLTS.Moves.Renaming.t

  type op_move = { o: TypingLTS.Moves.move; p: TypingLTS.Moves.move }
  type view_play = op_move list

  val pp_view_play : Format.formatter -> view_play -> unit

  (* The view current at a name's introduction, with the name's view-local
     level in it. *)
  (* [o_support] interprets a Player move's in-view free names in the ambient
     context, and is rebased only at use. *)
  type pointed_view = {
    view_play: view_play;
    subject: TypingLTS.Moves.Renaming.Namectx.Names.name;
    o_support: View.t;
  }

  val pp_pointed_view : Format.formatter -> pointed_view -> unit
  val pointed_view_to_yojson : pointed_view -> Yojson.Safe.t

  (* The argument of the view function: a P-view,
     the pattern being that move localized on its subject, with the guard read
     off the memory. *)
  type argument = {
    view_play: view_play;
    subject: TypingLTS.Moves.Renaming.Namectx.Names.name;
    pattern: TypingLTS.Moves.move;
    guard: ExtraMemory.pattern;
  }

  (* the abstract type for the view function *)
  type t

  val empty : t
  val pp : Format.formatter -> t -> unit

  val initial_pointed_view :
    TypingLTS.position ->
    TypingLTS.Moves.Renaming.Namectx.Names.name ->
    pointed_view

  (* Remap the subject to its stored view-local level and take the local
     form; any other free name is a polymorphic name given back, outside the
     fragment. *)
  val localize_incoming_move :
    pointed_view -> TypingLTS.Moves.move -> TypingLTS.Moves.move

  (* Take a Player move to the coordinates of a stored one, at the P-view's
     O-support restored in the ambient Opponent context and the reading of
     the provided context. *)
  val localize_player_move :
    View.t ->
    TypingLTS.Moves.Renaming.t ->
    TypingLTS.Moves.move ->
    TypingLTS.Moves.move

  (* The stored Player move read back in the ambient context, its provided
     levels through the current reading; None when one of them is beyond
     it. *)
  val instantiate_player_move :
    View.t ->
    TypingLTS.Moves.Renaming.t ->
    TypingLTS.Moves.move ->
    TypingLTS.Moves.move option

  (* The guard is consulted only when the P-views at (view, subject,
     pattern) disagree. *)
  val player_move_at :
    t ->
    view_play ->
    TypingLTS.Moves.Renaming.Namectx.Names.name ->
    TypingLTS.Moves.move ->
    ExtraMemory.state ->
    TypingLTS.Moves.move option

  (* The view function curried at (view, subject): its P-views there, by
     pattern then by guard, in recording order. *)
  val guarded_branches_at :
    t ->
    view_play ->
    TypingLTS.Moves.Renaming.Namectx.Names.name ->
    ( TypingLTS.Moves.move,
      (ExtraMemory.pattern, TypingLTS.Moves.move) Util.Pmap.pmap )
    Util.Pmap.pmap

  (* The same, keeping at each pattern the one Player move its guards agree
     on; fails where they differ. *)
  val innocent_branches_at :
    t ->
    view_play ->
    TypingLTS.Moves.Renaming.Namectx.Names.name ->
    (TypingLTS.Moves.move, TypingLTS.Moves.move) Util.Pmap.pmap

  (* The P-names of a view: the initial Player context, display hints
     erased, followed by the names introduced by the view's Player moves. *)
  val player_context_of_view :
    TypingLTS.Moves.Renaming.Namectx.t ->
    view_play ->
    TypingLTS.Moves.Renaming.Namectx.t

  (* Dually, the O-names: the initial Opponent context, display hints erased,
     followed by the names the view's Opponent moves introduce. *)
  val opponent_context_of_view :
    TypingLTS.Moves.Renaming.Namectx.t ->
    view_play ->
    TypingLTS.Moves.Renaming.Namectx.t

  (* The weakening of a Player move's local context into the Player context
     of the view it is played at. *)
  val fresh_names_weakening :
    TypingLTS.Moves.Renaming.Namectx.t ->
    view_play ->
    TypingLTS.Moves.move ->
    TypingLTS.Moves.Renaming.t

  (* A duplicate P-view is a no-op; a conflicting one fails: the chosen
     memory's guards do not determine the behavior there. *)
  val add_player_move : t -> argument -> TypingLTS.Moves.move -> t

  (* The final memory state is returned for the synthesis to derive its
     store typing from. *)
  val add_play : t -> Play.t -> t * ExtraMemory.state
end

module Make
    (A_nf : Lang.Interactive.A_NF)
    (TypingLTS :
      Typing.LTS
        with module Moves.Renaming = A_nf.IEnv.Renaming
         and type Moves.copattern =
          A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t)
    (ExtraMemory :
      Extra_memory.EXTRA_MEMORY
        with type move = TypingLTS.Moves.move
         and type name = TypingLTS.Moves.Renaming.Namectx.Names.name
         and type namectx = TypingLTS.Moves.Renaming.Namectx.t
         and type renaming = TypingLTS.Moves.Renaming.t) :
  VIEWFUNCTION
    with module TypingLTS = TypingLTS
     and module ExtraMemory = ExtraMemory = struct
  module TypingLTS = TypingLTS
  module ExtraMemory = ExtraMemory
  module Moves = TypingLTS.Moves
  module Renaming = Moves.Renaming
  module Namectx = Renaming.Namectx
  module Play = Play.Make (TypingLTS)
  module View = View.Make (Moves.Renaming)

  type op_move = { o: Moves.move; p: Moves.move }
  type view_play = op_move list

  let pp_op_move fmt op_move =
    Format.fprintf fmt "%a ↦ %a" Moves.pp_move op_move.o Moves.pp_move op_move.p

  let pp_view_play fmt = function
    | [] -> Format.pp_print_string fmt "ε"
    | view_play ->
        let pp_sep fmt () = Format.fprintf fmt "@ · " in
        Format.fprintf fmt "@[%a@]"
          (Format.pp_print_list ~pp_sep pp_op_move)
          view_play

  type pointed_view = {
    view_play: view_play;
    subject: Namectx.Names.name;
    o_support: View.t;
  }

  let pp_pointed_view fmt pointed_view =
    Format.fprintf fmt "@[⟨%a in %a |@, O: %a⟩@]" Namectx.Names.pp_name
      pointed_view.subject pp_view_play pointed_view.view_play Moves.Renaming.pp
      pointed_view.o_support

  let pointed_view_to_yojson pointed_view =
    `Assoc
      [
        ( "viewPlay",
          `List
            (List.map
               (fun op_move ->
                 `Assoc
                   [
                     ("o", Moves.move_to_yojson op_move.o);
                     ("p", Moves.move_to_yojson op_move.p);
                   ])
               pointed_view.view_play) );
        ("subject", Namectx.Names.name_to_yojson pointed_view.subject);
        ("oSupport", View.to_yojson pointed_view.o_support);
      ]

  type argument = {
    view_play: view_play;
    subject: Namectx.Names.name;
    pattern: Moves.move;
    guard: ExtraMemory.pattern;
  }

  type t = (argument, Moves.move) Util.Pmap.pmap

  let empty = Util.Pmap.empty

  let pp fmt strategy =
    let pp_empty fmt () = Format.pp_print_string fmt "⋅" in
    let pp_p_view fmt (argument, player_move) =
      Format.fprintf fmt "@[(%a, %a, %a, %a) ↦ %a@]" pp_view_play
        argument.view_play Namectx.Names.pp_name argument.subject Moves.pp_move
        argument.pattern ExtraMemory.pp_pattern argument.guard Moves.pp_move
        player_move in
    Util.Pmap.pp_pmap ~pp_empty pp_p_view fmt strategy

  let initial_pointed_view position name =
    {
      view_play= [];
      subject= name;
      o_support= Moves.Renaming.id (TypingLTS.get_namectxO position);
    }

  let localize_incoming_move (pointed_view : pointed_view) (move : Moves.move) :
      Moves.move =
    let ambient_subject = Moves.get_subject_name move in
    let remap name =
      if name = ambient_subject then pointed_view.subject
      else
        Util.Error.failwithf
          "View_function: the name %a occurs free besides the subject %a; a \
           polymorphic name given back is outside the fragment."
          Namectx.Names.pp_name name Namectx.Names.pp_name ambient_subject in
    Moves.erase_display_hints (Moves.map_free_names remap move)

  (* The weakening of the provided context into the context of a stored
     Player move's free names, after the view's Opponent names. *)
  let provided_context_weakening o_support provided_context =
    Moves.Renaming.weak_r provided_context (Moves.Renaming.dom o_support)

  let localize_player_move o_support provided_reading (move : Moves.move) :
      Moves.move =
    let weakening =
      provided_context_weakening o_support (Moves.Renaming.dom provided_reading)
    in
    let convert name =
      match Moves.Renaming.lookup_inv o_support name with
      | Some level -> level
      | None -> begin
          match Moves.Renaming.lookup_inv provided_reading name with
          | Some provided_level ->
              Moves.Renaming.lookup weakening provided_level
          | None ->
              Util.Error.failwithf
                "View_function: the name %a is neither in the view nor \
                 provided; outside the fragment."
                Namectx.Names.pp_name name
        end in
    Moves.erase_display_hints (Moves.map_free_names convert move)

  let instantiate_player_move o_support provided_reading (move : Moves.move) =
    let weakening =
      provided_context_weakening o_support (Moves.Renaming.dom provided_reading)
    in
    let missing = ref false in
    let instantiate level =
      if Moves.Renaming.is_in_dom o_support level then
        Moves.Renaming.lookup o_support level
      else
        match Moves.Renaming.lookup_inv weakening level with
        | Some provided_level ->
            Moves.Renaming.lookup provided_reading provided_level
        | None ->
            missing := true;
            level in
    let instantiated = Moves.map_free_names instantiate move in
    if !missing then None else Some instantiated

  let guarded_branches_at strategy view_play subject =
    Util.Pmap.fold
      (fun curried ((argument : argument), player_move) ->
        if argument.view_play = view_play && argument.subject = subject then
          let at_pattern =
            Option.value ~default:Util.Pmap.empty
              (Util.Pmap.lookup argument.pattern curried) in
          Util.Pmap.modadd
            ( argument.pattern,
              Util.Pmap.modadd (argument.guard, player_move) at_pattern )
            curried
        else curried)
      Util.Pmap.empty strategy

  let agreeing_player_move guarded_player_moves =
    match Util.Pmap.codom guarded_player_moves with
    | [] -> None
    | player_move :: others ->
        if List.for_all (( = ) player_move) others then Some player_move
        else None

  let player_move_at strategy view_play subject pattern state =
    let guarded_player_moves =
      Option.value ~default:Util.Pmap.empty
        (Util.Pmap.lookup pattern
           (guarded_branches_at strategy view_play subject)) in
    match agreeing_player_move guarded_player_moves with
    | Some player_move -> Some player_move
    | None ->
        Util.Pmap.lookup (ExtraMemory.guard_of_state state) guarded_player_moves

  let innocent_branches_at strategy view_play subject =
    Util.Pmap.map
      (fun (pattern, guarded_player_moves) ->
        match agreeing_player_move guarded_player_moves with
        | Some player_move -> (pattern, player_move)
        | None ->
            Util.Error.failwithf
              "View_function.innocent_branches_at: the P-views at the pattern \
               %a disagree; the strategy is not innocent there."
              Moves.pp_move pattern)
      (guarded_branches_at strategy view_play subject)

  let add_player_move strategy argument player_move =
    match Util.Pmap.lookup argument strategy with
    | Some recorded ->
        if recorded = player_move then strategy
        else
          failwith
            "View_function.add_player_move: conflicting Player moves at the \
             same argument"
    | None -> Util.Pmap.modadd (argument, player_move) strategy

  let player_context_of_view initial_namectxP view_play =
    List.fold_left
      (fun context op_move ->
        Namectx.concat context (Moves.get_namectx op_move.p))
      (Namectx.erase_display_hints initial_namectxP)
      view_play

  let opponent_context_of_view initial_namectxO view_play =
    List.fold_left
      (fun context op_move ->
        Namectx.concat context (Moves.get_namectx op_move.o))
      (Namectx.erase_display_hints initial_namectxO)
      view_play

  let fresh_names_weakening initial_namectxP view_play move =
    Renaming.weak_r (Moves.get_namectx move)
      (player_context_of_view initial_namectxP view_play)

  let add_play strategy play =
    let initial_position = Play.initial_position play in
    let initial_namectxP = TypingLTS.get_namectxP initial_position in
    let initial_pointed_views =
      Util.Pmap.list_to_pmap
        (List.map
           (fun name -> (name, initial_pointed_view initial_position name))
           (Namectx.get_names initial_namectxP)) in
    let insert_op_move (strategy, pointed_views, memory_state)
        (opponent_step : Play.step) (player_step : Play.step) =
      let opponent_move = opponent_step.move in
      let mid_position = opponent_step.target in
      let ambient_player_move = player_step.move in
      let ambient_subject = Moves.get_subject_name opponent_move in
      let pointed_view =
        match Util.Pmap.lookup ambient_subject pointed_views with
        | Some (pointed_view : pointed_view) -> pointed_view
        | None ->
            Util.Error.failwithf
              "View_function.add_play: the name %a has no pointed view. Please \
               report."
              Namectx.Names.pp_name ambient_subject in
      let mid_namectxO = TypingLTS.get_namectxO mid_position in
      let o_support =
        View.restore_view pointed_view.o_support mid_namectxO
          (Moves.fresh_names opponent_step.weakening opponent_move) in
      let pattern = localize_incoming_move pointed_view opponent_move in
      let player_move =
        localize_player_move o_support
          (ExtraMemory.provided_reading memory_state mid_namectxO)
          ambient_player_move in
      let strategy =
        add_player_move strategy
          {
            view_play= pointed_view.view_play;
            subject= pointed_view.subject;
            pattern;
            guard= ExtraMemory.guard_of_state memory_state;
          }
          player_move in
      let extended_view =
        pointed_view.view_play @ [ { o= pattern; p= player_move } ] in
      let fresh_pointed_views =
        List.map2
          (fun ambient_name view_level ->
            ( ambient_name,
              { view_play= extended_view; subject= view_level; o_support } ))
          (Moves.fresh_names player_step.weakening ambient_player_move)
          (Moves.fresh_names
             (fresh_names_weakening initial_namectxP pointed_view.view_play
                player_move)
             player_move) in
      ( strategy,
        Util.Pmap.concat pointed_views
          (Util.Pmap.list_to_pmap fresh_pointed_views),
        ExtraMemory.advance opponent_step.weakening opponent_move memory_state
      ) in
    let rec insert_op_moves ((strategy, _, memory_state) as recording) play =
      match Play.opponent_step play with
      | None -> (strategy, memory_state)
      | Some (opponent_step, rest) ->
          let (player_step, rest) = Play.player_step rest in
          insert_op_moves
            (insert_op_move recording opponent_step player_step)
            rest in
    insert_op_moves
      (strategy, initial_pointed_views, ExtraMemory.initial_state)
      play
end

(* The view function run as an interactive language: γ binds each Player
   name to its pointed view. *)
module MakeLang
    (A_nf : Lang.Interactive.TYPED_A_NF)
    (ViewFunction :
      VIEWFUNCTION
        with type TypingLTS.Moves.copattern =
          A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t
         and module TypingLTS.Moves.Renaming = A_nf.IEnv.Renaming
         and module TypingLTS.BranchMonad = A_nf.BranchMonad
         and type TypingLTS.store_ctx = A_nf.Storectx.t)
    (EvalMonad : Util.Monad.RUNNABLE) =
struct
  module Strategy = struct
    module TypingLTS = ViewFunction.TypingLTS
    module Moves = TypingLTS.Moves
    module Renaming = Moves.Renaming

    type abstract_normal_form = A_nf.abstract_normal_form

    (* The initial Player context is what the view-local levels of introduced
       Player names are counted from. *)
    type t = {
      view_function: ViewFunction.t; (* immutable during the run *)
      initial_namectxP: Renaming.Namectx.t;
      memory: ViewFunction.ExtraMemory.state;
    }

    let pp fmt strategy =
      Format.fprintf fmt "@[⟨%a |@, Memory: %a⟩@]" ViewFunction.pp
        strategy.view_function ViewFunction.ExtraMemory.pp_state strategy.memory

    type value = ViewFunction.pointed_view

    let pp_value = ViewFunction.pp_pointed_view
    let value_to_yojson = ViewFunction.pointed_view_to_yojson

    (* The initial move's names are the initial Player context, so they are
       their own levels. *)
    let initial_values _strategy namectxO local_namectx =
      List.map
        (fun name ->
          {
            ViewFunction.view_play= [];
            subject= name;
            o_support= Moves.Renaming.id namectxO;
          })
        (Renaming.Namectx.get_names local_namectx)

    let answer strategy subject_view namectxO weakening move =
      let pattern = ViewFunction.localize_incoming_move subject_view move in
      let o_support =
        ViewFunction.View.restore_view subject_view.ViewFunction.o_support
          namectxO
          (Moves.fresh_names weakening move) in
      Option.bind
        (ViewFunction.player_move_at strategy.view_function
           subject_view.ViewFunction.view_play subject_view.ViewFunction.subject
           pattern strategy.memory) (fun player_move ->
          Option.map
            (fun (instantiated_player_move : Moves.move) ->
              let extended_view =
                subject_view.ViewFunction.view_play
                @ [ { ViewFunction.o= pattern; p= player_move } ] in
              let pointed_views =
                List.map
                  (fun view_level ->
                    {
                      ViewFunction.view_play= extended_view;
                      subject= view_level;
                      o_support;
                    })
                  (Moves.fresh_names
                     (ViewFunction.fresh_names_weakening
                        strategy.initial_namectxP
                        subject_view.ViewFunction.view_play player_move)
                     instantiated_player_move) in
              ( instantiated_player_move,
                pointed_views,
                {
                  strategy with
                  memory=
                    ViewFunction.ExtraMemory.advance weakening move
                      strategy.memory;
                } ))
            (ViewFunction.instantiate_player_move o_support
               (ViewFunction.ExtraMemory.provided_reading strategy.memory
                  namectxO)
               player_move))
  end

  include
    Strategy_language.Make (A_nf) (ViewFunction.TypingLTS) (Strategy)
      (EvalMonad)

  let initial_store view_function position =
    initial_store
      {
        Strategy.view_function;
        initial_namectxP= ViewFunction.TypingLTS.get_namectxP position;
        memory= ViewFunction.ExtraMemory.initial_state;
      }
      position
end
