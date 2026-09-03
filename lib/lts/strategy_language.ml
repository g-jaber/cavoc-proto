(* A strategy run as an interactive language over the abstract normal forms
   of the language it was built over: the term is the pending move, the store
   the strategy with its run's state. *)
(* The transparent copattern is what lets the machine of the underlying typing
   LTS run this language with no wrapping layer. *)

(* What a strategy supplies to the machine: its run state, what the
   environment binds each Player name to, and how it answers. *)
module type STRATEGY = sig
  module TypingLTS : Typing.LTS

  type abstract_normal_form
  type t

  val pp : Format.formatter -> t -> unit

  type value

  val pp_value : Format.formatter -> value -> unit
  val value_to_yojson : value -> Yojson.Safe.t

  (* The values of the names the initial move introduces, in the ambient
     Opponent context. *)
  val initial_values :
    t ->
    TypingLTS.Moves.Renaming.Namectx.t ->
    TypingLTS.Moves.Renaming.Namectx.t ->
    value list

  (* The answer to an Opponent move, given the weakening of its local context
     and the value its subject is bound to.
     The function provides the Player move in the ambient
     contexts, the values of the names it introduces, and the strategy after it. *)
  val answer :
    t ->
    value ->
    TypingLTS.Moves.Renaming.Namectx.t ->
    TypingLTS.Moves.Renaming.t ->
    TypingLTS.Moves.move ->
    (TypingLTS.Moves.move * value list * t) option
end

module Make
    (A_nf : Lang.Interactive.TYPED_A_NF)
    (TypingLTS :
      Typing.LTS
        with module Moves.Renaming = A_nf.IEnv.Renaming
         and module BranchMonad = A_nf.BranchMonad
         and type Moves.copattern =
          A_nf.abstract_normal_form * A_nf.IEnv.Renaming.Namectx.t
         and type store_ctx = A_nf.Storectx.t)
    (Strategy :
      STRATEGY
        with module TypingLTS = TypingLTS
         and type abstract_normal_form = A_nf.abstract_normal_form)
    (EvalMonad : Util.Monad.RUNNABLE) =
struct
  (* The abstract normal forms of LANG are the underlying language's, the
     environments are not, hence the substitution. *)
  include (
    A_nf :
      Lang.Interactive.A_NF
        with module IEnv := A_nf.IEnv
         and type abstract_normal_form = A_nf.abstract_normal_form)

  module Moves = TypingLTS.Moves
  module Renaming = Moves.Renaming
  module EvalMonad = EvalMonad
  module BranchMonad = TypingLTS.BranchMonad
  module Storectx = A_nf.Storectx

  (* The machine consumes only copairing, dom, im and empty of γ, never
     embed_name. *)
  module IEnv =
    Lang.Ienv.Make_PMAP
      (Renaming)
      (struct
        type t = Strategy.value

        let to_yojson = Strategy.value_to_yojson
        let renam_act _renaming value = value

        let embed_name _ =
          failwith
            "No strategy value is derivable from a bare name. Please report."

        let pp = Strategy.pp_value
      end)

  type store = { strategy: Strategy.t; storectx: Storectx.t }

  let pp_store fmt store = Strategy.pp fmt store.strategy
  let string_of_store = Format.asprintf "%a" pp_store
  let store_to_yojson store = `String (string_of_store store)

  let initial_store strategy position =
    { strategy; storectx= TypingLTS.get_storectx position }

  let infer_type_store store = store.storectx

  (* The pending move [concretize_a_nf] packages for eval, which receives no
     environment: either an Opponent move with the value of its subject, or
     the initial move the strategy opens with. *)
  type pending_move =
    | OpponentMove of {
        move: Moves.move;
        weakening: Renaming.t;
        namectxP: Renaming.Namectx.t;
            (* the Player context then, for printing *)
        subject_value: Strategy.value;
      }
    | InitialMove of Moves.move

  type opconf = pending_move * store

  let pp_opconf fmt (pending_move, store) =
    match pending_move with
    | OpponentMove { move; weakening; namectxP; _ } ->
        Format.fprintf fmt "⟨%a | %a⟩"
          (Moves.pp_move_in
             ~show_name:(Renaming.Namectx.show_name_in namectxP)
             weakening)
          move pp_store store
    | InitialMove initial_move ->
        Format.fprintf fmt "⟨initial move %a | %a⟩" Moves.pp_move initial_move
          pp_store store

  let string_of_opconf = Format.asprintf "%a" pp_opconf

  let initial_move_opconf initial_move store =
    (InitialMove (Moves.erase_display_hints initial_move), store)

  let generate_a_nf = A_nf.generate_a_nf
  let type_check_a_nf = A_nf.type_check_a_nf

  let bind_fresh_names namectxO local_namectx values =
    List.fold_left2
      (fun ienv local_name value ->
        let typ = Renaming.Namectx.lookup_exn local_namectx local_name in
        snd (IEnv.add_fresh ienv "" typ value))
      (IEnv.empty namectxO)
      (Renaming.Namectx.get_names local_namectx)
      values

  let eval ((pending_move, store), namectxO, storectx) =
    match pending_move with
    | InitialMove initial_move ->
        let local_namectx = Moves.get_namectx initial_move in
        EvalMonad.return
          ( (fst initial_move, local_namectx, storectx),
            bind_fresh_names namectxO local_namectx
              (Strategy.initial_values store.strategy namectxO local_namectx),
            store )
    | OpponentMove { move; weakening; subject_value; _ } -> begin
        match
          Strategy.answer store.strategy subject_value namectxO weakening move
        with
        | None -> EvalMonad.stop ()
        | Some ((a_nf, local_namectx), values, strategy) ->
            EvalMonad.return
              ( (a_nf, local_namectx, storectx),
                bind_fresh_names namectxO local_namectx values,
                { store with strategy } )
      end

  (* Accepting an Opponent move extends the environment's image, where the
     machine reads the current Γ_O. *)
  let concretize_a_nf store ienv (a_nf, weakening) =
    let move = (a_nf, Renaming.dom weakening) in
    ( ( OpponentMove
          {
            move;
            weakening;
            namectxP= IEnv.dom ienv;
            subject_value= IEnv.lookup_exn ienv (Moves.get_subject_name move);
          },
        store ),
      IEnv.weaken_r ienv (Moves.get_namectx move) )
end
