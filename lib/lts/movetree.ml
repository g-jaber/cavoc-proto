module type MOVETREE = sig
  module TypingLTS : Typing.LTS
  module Play : module type of Play.Make (TypingLTS)
  module View : module type of View.Make (TypingLTS.Moves.Renaming)

  type t = { branches: branch list }

  and branch = {
    opponent_move: TypingLTS.Moves.move;
    player_move: TypingLTS.Moves.move;
    continuation: t;
  }

  (* The bookkeeping of Vis_lts.MakeNameIndexed run on both polarities, so
     that both moves of an op_move can be converted to view-local levels. *)
  type full_view = {
    player_view: TypingLTS.Moves.Renaming.t;
    player_views_at_opponent_names: View.view_map;
    opponent_views_at_player_names: View.view_map;
  }

  val empty : t
  val pp : Format.formatter -> t -> unit
  val add_play : t -> Play.t -> t
  val initial_full_view : TypingLTS.position -> full_view

  (* The move read in the view: its free names at the view's levels, at the
     empty ambient context; every free name must lie in the view's image. *)
  val view_local_move :
    TypingLTS.Moves.Renaming.t -> TypingLTS.Moves.move -> TypingLTS.Moves.move
end

module Make
    (A_nf : Lang.Interactive.A_NF)
    (TypingLTS :
      Typing.LTS
        with module Moves.Renaming = A_nf.IEnv.Renaming
         and type Moves.copattern =
          A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t) :
  MOVETREE with module TypingLTS = TypingLTS = struct
  module TypingLTS = TypingLTS
  module Moves = TypingLTS.Moves
  module Play = Play.Make (TypingLTS)
  module View = View.Make (Moves.Renaming)

  type t = { branches: branch list }

  and branch = {
    opponent_move: Moves.move;
    player_move: Moves.move;
    continuation: t;
  }

  type full_view = {
    player_view: Moves.Renaming.t;
    player_views_at_opponent_names: View.view_map;
    opponent_views_at_player_names: View.view_map;
  }

  let empty = { branches= [] }

  let view_local_move view move =
    let view_level name =
      match Moves.Renaming.lookup_inv view name with
      | Some level -> level
      | None ->
          Util.Error.failwithf
            "Movetree: the name %a occurs free outside the view %a"
            Moves.Renaming.Namectx.Names.pp_name name Moves.Renaming.pp view
    in
    Moves.erase_display_hints (Moves.map_free_names view_level move)

  let rec pp fmt node =
    match node.branches with
    | [] -> ()
    | branches ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        let pp_branch fmt branch =
          Format.fprintf fmt "%a ↦ %a%a" Moves.pp_move branch.opponent_move
            Moves.pp_move branch.player_move pp branch.continuation in
        Format.fprintf fmt "@[{%a}@]"
          (Format.pp_print_list ~pp_sep pp_branch)
          branches

  let initial_full_view position =
    let namectxP = TypingLTS.get_namectxP position in
    let namectxO = TypingLTS.get_namectxO position in
    {
      player_view= Moves.Renaming.id namectxP;
      player_views_at_opponent_names=
        View.init_view_map
          (Moves.Renaming.id namectxP)
          (Moves.Renaming.Namectx.get_names namectxO);
      opponent_views_at_player_names=
        View.init_view_map
          (Moves.Renaming.id namectxO)
          (Moves.Renaming.Namectx.get_names namectxP);
    }

  let advance full_view (opponent_step : Play.step) (player_step : Play.step) =
    let opponent_move = opponent_step.move in
    let mid_position = opponent_step.target in
    let player_move = player_step.move in
    let player_view =
      View.transport_to_context full_view.player_view
        (TypingLTS.get_namectxP mid_position) in
    let view_local_opponent_move = view_local_move player_view opponent_move in
    let fresh_opponent_names =
      Moves.fresh_names opponent_step.weakening opponent_move in
    let player_move_view =
      View.restore_view_at_subject full_view.opponent_views_at_player_names
        (Moves.get_subject_name opponent_move)
        (TypingLTS.get_namectxO mid_position)
        fresh_opponent_names in
    let view_local_player_move = view_local_move player_move_view player_move in
    let fresh_player_names =
      Moves.fresh_names player_step.weakening player_move in
    let player_views_at_opponent_names =
      View.record_view_at_introduction full_view.player_views_at_opponent_names
        player_view fresh_opponent_names in
    let opponent_views_at_player_names =
      View.record_view_at_introduction full_view.opponent_views_at_player_names
        player_move_view fresh_player_names in
    let player_view =
      View.restore_view_at_subject player_views_at_opponent_names
        (Moves.get_subject_name player_move)
        (TypingLTS.get_namectxP player_step.target)
        fresh_player_names in
    ( view_local_opponent_move,
      view_local_player_move,
      {
        player_view;
        player_views_at_opponent_names;
        opponent_views_at_player_names;
      } )

  let rec insert_op_moves node full_view play =
    match Play.opponent_step play with
    | None -> node
    | Some (opponent_step, rest) ->
        let (player_step, rest) = Play.player_step rest in
        let (view_local_opponent_move, player_move, full_view) =
          advance full_view opponent_step player_step in
        {
          branches=
            insert_branch view_local_opponent_move player_move full_view rest
              node.branches;
        }

  and insert_branch view_local_opponent_move player_move full_view rest =
    function
    | [] ->
        [
          {
            opponent_move= view_local_opponent_move;
            player_move;
            continuation= insert_op_moves empty full_view rest;
          };
        ]
    | branch :: branches when branch.opponent_move = view_local_opponent_move ->
        if branch.player_move <> player_move then
          failwith
            "Movetree.add_play: conflicting Player moves for an Opponent branch";
        {
          branch with
          continuation= insert_op_moves branch.continuation full_view rest;
        }
        :: branches
    | branch :: branches ->
        branch
        :: insert_branch view_local_opponent_move player_move full_view rest
             branches

  let add_play tree play =
    insert_op_moves tree (initial_full_view (Play.initial_position play)) play
end

(* The movetree run as an interactive language: the run state is the
   remaining tree with its full view, and γ binds Player names to nothing. *)
module MakeLang
    (A_nf : Lang.Interactive.TYPED_A_NF)
    (Movetree :
      MOVETREE
        with module TypingLTS.Moves.Renaming = A_nf.IEnv.Renaming
         and module TypingLTS.BranchMonad = A_nf.BranchMonad
         and type TypingLTS.Moves.copattern =
          A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t
         and type TypingLTS.store_ctx = A_nf.Storectx.t)
    (EvalMonad : Util.Monad.RUNNABLE) =
struct
  module Strategy = struct
    module TypingLTS = Movetree.TypingLTS
    module Moves = TypingLTS.Moves
    module Renaming = Moves.Renaming

    type abstract_normal_form = A_nf.abstract_normal_form
    type t = { node: Movetree.t; full_view: Movetree.full_view }

    let pp fmt strategy =
      Format.fprintf fmt "@[⟨%a |@, View: %a⟩@]" Movetree.pp strategy.node
        Moves.Renaming.pp strategy.full_view.Movetree.player_view

    type value = unit

    let pp_value fmt () = Format.pp_print_string fmt "⋅"
    let value_to_yojson () = `Null

    let initial_values _strategy _namectxO local_namectx =
      List.map (fun _ -> ()) (Renaming.Namectx.get_names local_namectx)

    let answer strategy () namectxO weakening incoming_move =
      let full_view = strategy.full_view in
      let view_local_opponent_move =
        Movetree.view_local_move full_view.Movetree.player_view incoming_move
      in
      Option.map
        (fun branch ->
          let fresh_opponent_names = Moves.fresh_names weakening incoming_move in
          let player_move_view =
            Movetree.View.restore_view_at_subject
              full_view.Movetree.opponent_views_at_player_names
              (Moves.get_subject_name incoming_move)
              namectxO fresh_opponent_names in
          let player_views_at_opponent_names =
            Movetree.View.record_view_at_introduction
              full_view.Movetree.player_views_at_opponent_names
              full_view.Movetree.player_view fresh_opponent_names in
          let namectxP = Moves.Renaming.im full_view.Movetree.player_view in
          let player_move =
            Moves.map_free_names
              (Moves.Renaming.lookup player_move_view)
              branch.Movetree.player_move in
          let player_move_namectx = Moves.get_namectx player_move in
          let fresh_player_names =
            Moves.fresh_names
              (Renaming.weak_r player_move_namectx namectxP)
              player_move in
          let opponent_views_at_player_names =
            Movetree.View.record_view_at_introduction
              full_view.Movetree.opponent_views_at_player_names player_move_view
              fresh_player_names in
          let player_view =
            Movetree.View.restore_view_at_subject player_views_at_opponent_names
              (Moves.get_subject_name player_move)
              (Renaming.Namectx.concat namectxP player_move_namectx)
              fresh_player_names in
          ( player_move,
            List.map
              (fun _ -> ())
              (Renaming.Namectx.get_names player_move_namectx),
            {
              node= branch.Movetree.continuation;
              full_view=
                {
                  Movetree.player_view;
                  player_views_at_opponent_names;
                  opponent_views_at_player_names;
                };
            } ))
        (List.find_opt
           (fun branch ->
             branch.Movetree.opponent_move = view_local_opponent_move)
           strategy.node.Movetree.branches)
  end

  include
    Strategy_language.Make (A_nf) (Movetree.TypingLTS) (Strategy) (EvalMonad)

  let initial_store node position =
    initial_store
      { Strategy.node; full_view= Movetree.initial_full_view position }
      position
end

module _ : functor
  (A_nf : Lang.Interactive.TYPED_A_NF)
  (Movetree : MOVETREE
                with module TypingLTS.Moves.Renaming = A_nf.IEnv.Renaming
                 and module TypingLTS.BranchMonad = A_nf.BranchMonad
                 and type TypingLTS.Moves.copattern =
                  A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t
                 and type TypingLTS.store_ctx = A_nf.Storectx.t)
  (EvalMonad : Util.Monad.RUNNABLE)
  -> Lang.Interactive.LANG =
  MakeLang
