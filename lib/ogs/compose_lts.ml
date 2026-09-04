(* The sequential composition of two OGS components from source: the
   module implements the signature the client is written against. *)

module type COMPOSITION_WITH_INIT = sig
  include Lts.Strategy.LTS

  type comp_move

  val comp_move_is_sync : comp_move -> bool
  val string_of_comp_move_from : active_conf -> comp_move -> string
  val par_p_trans : active_conf -> (comp_move * conf) EvalMonad.m

  (* [imported_sig] is a second lexing buffer on the module's signature,
     read as the client's imports. *)
  val lexing_init_pconf :
    module_implem:Lexing.lexbuf ->
    module_sig:Lexing.lexbuf ->
    client_implem:Lexing.lexbuf ->
    client_sig:Lexing.lexbuf ->
    imported_sig:Lexing.lexbuf ->
    passive_conf
end

module Make
    (IntLang : Lang.Interactive.LANG_WITH_INIT)
    (PlainTypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = IntLang.IEnv.Renaming
         and type Moves.copattern =
          IntLang.abstract_normal_form * IntLang.IEnv.Renaming.Namectx.t
         and type store_ctx = IntLang.Storectx.t)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves = PlainTypingLTS.Moves
         and type store_ctx = IntLang.Storectx.t) :
  COMPOSITION_WITH_INIT
    with module TypingLTS = TypingLTS
     and type 'a EvalMonad.r = 'a IntLang.EvalMonad.r = struct
  module Component = Ogslts.MakeWithInit (IntLang) (PlainTypingLTS)

  module Composition =
    Lts.Sequential_compose.Make (PlainTypingLTS) (TypingLTS) (Component)
      (Component)

  module Namectx = IntLang.IEnv.Renaming.Namectx
  include Composition

  let comp_move_is_sync = function
    | Composition.SyncMove _ -> true
    | Composition.ExternalMove _ -> false

  let lexing_init_pconf ~module_implem ~module_sig ~client_implem ~client_sig
      ~imported_sig =
    let module_pconf = Component.lexing_init_pconf module_implem module_sig in
    let client_pconf =
      Component.lexing_init_pconf ~opponent_signature:imported_sig client_implem
        client_sig in
    let initial_position namectxP =
      PlainTypingLTS.init_pas_pos IntLang.Storectx.empty namectxP Namectx.empty
    in
    let module_pos = Component.get_passive_pos module_pconf in
    let client_pos = Component.get_passive_pos client_pconf in
    let shared_position =
      initial_position (PlainTypingLTS.get_namectxP module_pos) in
    let client_position =
      initial_position (PlainTypingLTS.get_namectxP client_pos) in
    let module_position =
      initial_position (PlainTypingLTS.get_namectxO module_pos) in
    Composition.initial_pconf IntLang.Storectx.empty ~client_pconf
      ~client_position
      ~client_renaming:(identity_renaming (client_position, shared_position))
      ~shared_position ~module_pconf ~module_position
      ~module_renaming:(identity_renaming (shared_position, module_position))
end
