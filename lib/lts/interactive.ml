module type INT = sig
  (* To be instantiated *)
  module GameLTS : Gamelts.GAME

  module IntLang :
    Lang.Interactive.LANG
      with type Name.name = GameLTS.Moves.name
       and type abstract_normal_form = GameLTS.Moves.kdata
       and type name_ctx = GameLTS.name_ctx
       and type store_ctx = GameLTS.store_ctx

  module Name : Lang.Names.CONT_NAMES with type name = IntLang.Name.name

  (* generate_output_move Γₒ nf returns a triple (m,γ,Δ,Γₒ')
      where the output move m is formed by
      the abstracted normal form a_nf build from nf,
      such that a_nf{γ} = nf and Γₒ ⊢ γ:Δ and Γₒ' = Γₒ + Δ.
      We do not take into account disclosure of locations currently.
      We return Γₒ' to be ready to handle linear ressources in the future. *)
  val generate_output_move :
    GameLTS.position ->
    IntLang.normal_form ->
    (GameLTS.Moves.move
    * IntLang.interactive_env
    * IntLang.name_ctx
    * GameLTS.position)
    IntLang.EvalMonad.m

  (* trigger_computation γ m returns (t,μ,γ).
     We might return an interactive environment γ' distinct of γ
     once taking into account linear resources.*)
  val trigger_computation :
    IntLang.store ->
    IntLang.interactive_env ->
    GameLTS.Moves.move ->
    IntLang.opconf * IntLang.interactive_env
end

module Make (IntLang : Lang.Interactive.LANG) :
  INT
    with type GameLTS.Moves.name = IntLang.Name.name
     and type GameLTS.name_ctx = IntLang.name_ctx = struct
  module GameLTS = Gamelts.Make (IntLang)
  module IntLang = IntLang
  module Name = IntLang.Name

  let generate_output_move ictx nf =
    match
      IntLang.abstracting_nf nf
        (GameLTS.get_namectxO ictx)
        (GameLTS.get_storectx ictx)
    with
    | Some (a_nf, ienv, lnamectx, storectx) ->
        let move = GameLTS.Moves.build (GameLTS.Moves.Output, a_nf) in
        let ictx' = GameLTS.trigger_move ictx move lnamectx storectx in
        IntLang.EvalMonad.return (move, ienv, lnamectx, ictx')
    | None ->
        Util.Error.failwithf
          "Error: the normal form %a is not typeable in the name context %a. \
           Please report."
          IntLang.pp_normal_form nf IntLang.pp_name_ctx
          (GameLTS.get_namectxO ictx)

  let trigger_computation store ienv input_move =
    Util.Debug.print_debug "Triggering a new computation.";
    let kdata = GameLTS.Moves.get_kdata input_move in
    match
      ( GameLTS.Moves.get_direction input_move,
        IntLang.concretize_a_nf store ienv kdata )
    with
    | (GameLTS.Moves.Input, (opconf, ienv')) -> (opconf, ienv')
    | _ ->
        failwith
          ("Error: the move "
          ^ GameLTS.Moves.string_of_move input_move
          ^ " is not an Opponent move. Please report.")
end
