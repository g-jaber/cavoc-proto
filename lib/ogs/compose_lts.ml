(* The open composition of two OGS components, in its sequential shape: the
   provider implements the shared signature, the client is written against it
   and is the only one whose signature is public. *)
(* This module initializes the composition from source, building the two
   components and their initial sharing; the scheduler itself is Lts.Compose. *)

module type COMPOSITION_WITH_INIT = sig
  include Lts.Strategy.LTS

  (* The par layer of the composite, for front ends that show the internal
     chattering (doc/web.md): p_trans unrolled one move at a time,
     synchronizations surfacing as τ-tagged moves instead of being iterated
     away by the trace operator. Sync moves are not choices: a driver
     auto-plays them, and only consults the user at external moves. *)
  type comp_move

  val comp_move_is_sync : comp_move -> bool

  (* The display form of a comp_move played from the given active
     configuration: free names are shown as the sender names them — the
     active component's context for a sync move, the composite's own
     context for an external one. A synchronization is an Output of its
     sender and an Input of its receiver at once, so it is displayed
     unpolarized. *)
  val string_of_comp_move_from : active_conf -> comp_move -> string
  val par_p_trans : active_conf -> (comp_move * conf) EvalMonad.m

  (* [imported_sig] must be a second lexing buffer on the same file as
     [provider_sig]. The shared signature is read twice, and must be: once as
     the provider's own signature, turning its declarations into the
     provider's Proponent names, and once as the client's imports, turning
     the same declarations into the client's Opponent names. Those are
     distinct names in distinct contexts, and it is the span that relates
     them. *)
  val lexing_init_pconf :
    provider_implem:Lexing.lexbuf ->
    provider_sig:Lexing.lexbuf ->
    client_implem:Lexing.lexbuf ->
    client_sig:Lexing.lexbuf ->
    imported_sig:Lexing.lexbuf ->
    passive_conf
end

module Make
    (IntLang : Lang.Interactive.LANG_WITH_INIT)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = IntLang.IEnv.Renaming
         and type Moves.copattern =
          IntLang.abstract_normal_form * IntLang.IEnv.Renaming.Namectx.t
         and type store_ctx = IntLang.Storectx.t) :
  COMPOSITION_WITH_INIT
    with module TypingLTS = TypingLTS
     and type 'a EvalMonad.r = 'a IntLang.EvalMonad.r = struct
  module IntStructure = Lts.Compose.MakeIntStructure (IntLang) (TypingLTS)
  module IntLts = Ogslts.MakeWithInit (IntLang) (TypingLTS)
  module Composition = Lts.Compose.Make (IntStructure) (IntLts) (IntLts)
  module Namectx = IntLang.IEnv.Renaming.Namectx
  include Composition.Hide

  type comp_move = Composition.Par.comp_move

  let comp_move_is_sync = function
    | Composition.Par.SyncMove _ -> true
    | Composition.Par.ExternalMove _ -> false

  let string_of_comp_move_from = Composition.Par.string_of_comp_move_from

  (* Par's own conf sum is mapped onto Hide's: the two layers share their
     configurations, and a front end only ever holds Hide's. *)
  let par_p_trans aconf =
    let open EvalMonad in
    let* (cm, next) = Composition.Par.p_trans aconf in
    return
      ( cm,
        match next with
        | Composition.Par.Active aconf -> Active aconf
        | Composition.Par.Passive pconf -> Passive pconf )

  let lexing_init_pconf ~provider_implem ~provider_sig ~client_implem
      ~client_sig ~imported_sig =
    let provider = IntLts.lexing_init_pconf provider_implem provider_sig in
    let client =
      IntLts.lexing_init_pconf ~opponent_signature:imported_sig client_implem
        client_sig in
    let provider_pos = IntLts.get_passive_pos provider in
    let client_pos = IntLts.get_passive_pos client in
    let provider_exports = TypingLTS.get_namectxP provider_pos in
    let client_imports = TypingLTS.get_namectxO client_pos in
    (* Both name sets come from the same signature, and classify its
       declarations the same way, so they correspond one-to-one by declared
       identifier. *)
    let provided_by_left =
      List.map
        (fun exported ->
          let id = Namectx.show_name_in provider_exports exported in
          match
            List.find_opt
              (fun imported ->
                Namectx.show_name_in client_imports imported = id)
              (Namectx.get_names client_imports)
          with
          | Some imported -> (exported, imported)
          | None ->
              failwith
                ("The shared signature declares " ^ id
               ^ ", which the client did not import. Please report."))
        (Namectx.get_names provider_exports) in
    let sharing =
      {
        IntStructure.left_init_pos= provider_pos;
        right_init_pos= client_pos;
        provided_by_left;
        (* Sequential composition: the client provides nothing back. *)
        provided_by_right= [];
        initial_storectx= IntLang.Storectx.empty;
      } in
    Composition.init_pconf sharing provider client
end
