(* ================================================
   LTS_KIND: Type definitions and LTS construction
   ================================================
   Core types and builders for LTS configuration:
   - Type definitions: oplang, control_structure, restriction, kind_lts
   - build_lts: Instantiates the appropriate LTS module
   - build_intlts: Creates intermediate language LTS
   - build_oplang: Creates operational language module
   - build_interactive_build: Creates interactive evaluation builder
*)

type oplang = RefML [@@deriving yojson]
type control_structure = DirectStyle | CPS [@@deriving yojson]
type restriction = Visibility | WellBracketing [@@deriving yojson]

type kind_lts = {
  oplang: oplang;
  symbolic: bool;
  control: control_structure;
  restrictions: restriction list;
}
[@@deriving yojson]

(* This is only needed because parametrized types in package constraints
   (i.e. mod constraints in package types (first class modules)) are not
   supported by OCaml (as of 5.4) *)
module type SINGLE_OPLANG =
  Lang.Language.WITHAVAL_INOUT with type 'a EvalMonad.r = 'a

module type MULTI_OPLANG =
  Lang.Language.WITHAVAL_INOUT with type 'a EvalMonad.r = 'a list

module type SINGLE_INTLANG =
  Lang.Interactive.LANG_WITH_INIT with type 'a EvalMonad.r = 'a

module type MULTI_INTLANG =
  Lang.Interactive.LANG_WITH_INIT with type 'a EvalMonad.r = 'a list

module type SINGLE_RESULT_LTS_WITH_INIT =
  Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a

module type MULTI_RESULT_LTS_WITH_INIT =
  Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a list

module type SINGLE_RESULT_COMPOSITION_WITH_INIT =
  Ogs.Compose_lts.COMPOSITION_WITH_INIT with type 'a EvalMonad.r = 'a

module type SINGLE_RESULT_ARENA = sig
  module TypingLTS : Lts.Typing.LTS

  (* The arena a signature declares, with no implementation on either
     side. *)
  val initial_position : Lexing.lexbuf -> TypingLTS.position

  (* The moves offered at a position: the well-typed ones the play so far
     still leaves definable. *)
  (* The prototype does not consume answered continuations, so moves answering
     them are still offered though neither participant can implement them. *)
  val offered_moves :
    arena:TypingLTS.position ->
    TypingLTS.position ->
    TypingLTS.Moves.pol_move list ->
    (TypingLTS.Moves.pol_move * TypingLTS.Moves.Renaming.t * TypingLTS.position)
    list

  (* The two participants the play defines, the moves chronological and in the
     module's own polarity; None until that side has been played. *)
  val synthesize_module_source :
    TypingLTS.position -> TypingLTS.Moves.pol_move list -> string option

  val synthesize_client_source :
    TypingLTS.position -> TypingLTS.Moves.pol_move list -> string option
end

module type SINGLE_RESULT_LTS_WITH_CLIENT = sig
  include SINGLE_RESULT_LTS_WITH_INIT

  (* The source of the client whose play is the recorded one, None until the
     module has been played against; absent where no synthesis exists. *)
  val synthesize_client_source :
    (TypingLTS.position -> TypingLTS.Moves.pol_move list -> string option)
    option
end

let build_oplang kind : (module SINGLE_OPLANG) =
  match kind.oplang with
  | RefML -> (module Refml.RefML.WithAValConcrete (Util.Monad.ListB))

let build_oplang_multi kind : (module MULTI_OPLANG) =
  match kind.oplang with
  | RefML -> (module Refml.RefML.WithAValSymbolic (Util.Monad.ListB))

let build_intlang kind (module OpLang : SINGLE_OPLANG) : (module SINGLE_INTLANG)
    =
  match kind.control with
  | DirectStyle -> (module Lang.Direct.Make (OpLang))
  | CPS ->
      let module CpsLang = Lang.Cps.MakeComp (OpLang) () in
      (module Lang.Interactive.Make (CpsLang))

let build_intlang_multi kind (module OpLang : MULTI_OPLANG) :
    (module MULTI_INTLANG) =
  match kind.control with
  | DirectStyle -> (module Lang.Direct.Make (OpLang))
  | CPS ->
      let module CpsLang = Lang.Cps.MakeComp (OpLang) () in
      (module Lang.Interactive.Make (CpsLang))

(* The direct-style stack has no continuation names, which the client
   synthesis is written against. *)
let build_direct_style_lts kind : (module SINGLE_RESULT_LTS_WITH_CLIENT) =
  let (module OpLang) = build_oplang kind in
  let (module IntLang) = build_intlang kind (module OpLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  let module WithoutClientSynthesis
      (RunTypingLTS :
        Lts.Typing.LTS
          with module Moves = TypingLTS.Moves
           and type store_ctx = TypingLTS.store_ctx) =
  struct
    include Ogs.Ogslts.MakeWithInit (IntLang) (RunTypingLTS)

    let synthesize_client_source = None
  end in
  (* Direct style is intrinsically well-bracketed: well-bracketing needs no
     enforcement, and visibility is enforced by the stack-based LTS alone. *)
  if List.mem Visibility kind.restrictions then
    (module WithoutClientSynthesis (Ogs.Vis_lts.MakeStackBased (TypingLTS)))
  else (module WithoutClientSynthesis (TypingLTS))

(* The definability-equipped stack: the concrete CPS language, its typing LTS,
   and the two syntheses over it. *)
module MakeDefinabilityStack () = struct
  module OpLang = Refml.Definability.WithAValConcrete (Util.Monad.ListB)
  module CpsLang = Lang.Definability.MakeComp (OpLang) ()
  module IntLang = Lang.Interactive.Make (CpsLang)
  module TypingLTS = Ogs.Typing.Make (IntLang)

  (* The higher-order-store memory: Player moves may depend on the clock and
     on the names received so far, all reified into private cells. *)
  module Memory = Refml.Definability.HigherOrderStoreMemory (TypingLTS.Moves)
  module ViewFunction = Lts.View_function.Make (IntLang) (TypingLTS) (Memory)

  module Synthesis =
    Definability.Synthesis.Make (CpsLang.Definability) (ViewFunction) (Memory)

  module Namectx = IntLang.IEnv.Renaming.Namectx

  (* The syntheses run over the plain typing LTS whatever restriction the
     interaction runs under, the enriched ones sharing its moves. *)
  let plain_position ~storectx ~namectxP ~namectxO =
    TypingLTS.init_pas_pos storectx namectxP namectxO

  let identifier_of namectxP nn = Namectx.show_name_in namectxP nn

  (* The client's final move: the one answer on a continuation of type unit. *)
  let client_final_move =
    let continuation_namectx =
      snd (Namectx.add_fresh Namectx.empty "" (Either.Right Refml.Types.TUnit))
    in
    match
      TypingLTS.BranchMonad.run
        (TypingLTS.generate_moves
           (TypingLTS.init_act_pos IntLang.Storectx.empty Namectx.empty
              continuation_namectx))
    with
    | [ ((_, move), _, _) ] -> (continuation_namectx, move)
    | _ -> failwith "Definability: no single answer (). Please report."

  let synthesize_client_source ~storectx ~namectxP ~namectxO moves =
    Option.map Refml.Definability.string_of_source_term
      (Synthesis.synthesize_client_program_of_play ~final_move:client_final_move
         (plain_position ~storectx ~namectxP ~namectxO) moves (fun nn ->
           Refml.Syntax.Var (identifier_of namectxP nn)))

  let synthesize_module_source ~storectx ~namectxP ~namectxO moves =
    Option.map
      (fun exports ->
        Refml.Definability.source_of_definability_implementation
          ~exports:
            (List.map
               (fun (nn, value) -> (identifier_of namectxP nn, value))
               exports)
          ())
      (Synthesis.synthesize_module_program_of_play
         (plain_position ~storectx ~namectxP ~namectxO)
         moves)

  (* The concrete LTS over a typing LTS with the same moves, the client
     synthesized from the plays it records. *)
  module WithClientSynthesis
      (RunTypingLTS :
        Lts.Typing.LTS
          with module Moves = TypingLTS.Moves
           and type store_ctx = TypingLTS.store_ctx) =
  struct
    include Ogs.Ogslts.MakeWithInit (IntLang) (RunTypingLTS)

    let synthesize_client_source =
      Some
        (fun position ->
          synthesize_client_source
            ~storectx:(RunTypingLTS.get_storectx position)
            ~namectxP:(RunTypingLTS.get_namectxP position)
            ~namectxO:(RunTypingLTS.get_namectxO position))
  end
end

(* The CPS stack carries the definability-equipped language, so that the
   interaction can be synthesized back into a client program. *)
let build_cps_lts kind : (module SINGLE_RESULT_LTS_WITH_CLIENT) =
  let module Stack = MakeDefinabilityStack () in
  let module TypingLTS = Stack.TypingLTS in
  match
    ( List.mem WellBracketing kind.restrictions,
      List.mem Visibility kind.restrictions )
  with
  | (false, false) -> (module Stack.WithClientSynthesis (TypingLTS))
  | (true, false) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      (module Stack.WithClientSynthesis
                (Lts.Product_lts.Make (TypingLTS) (WBLTS)))
  | (false, true) ->
      (module Stack.WithClientSynthesis (Ogs.Vis_lts.MakeNameIndexed (TypingLTS)))
  | (true, true) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module ProductLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      (module Stack.WithClientSynthesis (Ogs.Vis_lts.MakeNameIndexed (ProductLTS)))

(* Both participants being synthesized, visibility and well-bracketing are
   forced here rather than read off the options, which the page locks. *)
let build_arena () : (module SINGLE_RESULT_ARENA) =
  let module Stack = MakeDefinabilityStack () in
  let module WBLTS = Ogs.Wblts.Make (Stack.TypingLTS.Moves) in
  let module ProductLTS = Lts.Product_lts.Make (Stack.TypingLTS) (WBLTS) in
  let module RunTypingLTS = Ogs.Vis_lts.MakeNameIndexed (ProductLTS) in
  (module struct
    module TypingLTS = RunTypingLTS

    let initial_position signature =
      RunTypingLTS.init_pas_pos Stack.IntLang.Storectx.empty
        (Stack.IntLang.get_typed_namectx signature)
        Stack.Namectx.empty

    let synthesize_at synthesize position =
      synthesize
        ~storectx:(RunTypingLTS.get_storectx position)
        ~namectxP:(RunTypingLTS.get_namectxP position)
        ~namectxO:(RunTypingLTS.get_namectxO position)

    let synthesize_module_source = synthesize_at Stack.synthesize_module_source
    let synthesize_client_source = synthesize_at Stack.synthesize_client_source

    let offered_moves ~arena position played =
      let keeps_the_play_definable move =
        let played = played @ [ move ] in
        try
          ignore (synthesize_module_source arena played);
          ignore (synthesize_client_source arena played);
          true
        with Failure _ -> false in
      List.filter
        (fun (move, _, _) -> keeps_the_play_definable move)
        (RunTypingLTS.BranchMonad.run (RunTypingLTS.generate_moves position))
  end)

let build_concrete_lts kind : (module SINGLE_RESULT_LTS_WITH_CLIENT) =
  match kind.control with
  | DirectStyle -> build_direct_style_lts kind
  | CPS -> build_cps_lts kind

let build_compose_lts kind : (module SINGLE_RESULT_COMPOSITION_WITH_INIT) =
  let (module OpLang) = build_oplang kind in
  let (module IntLang) = build_intlang kind (module OpLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  match
    ( List.mem WellBracketing kind.restrictions,
      List.mem Visibility kind.restrictions,
      kind.control )
  with
  | (false, false, _) -> (module Ogs.Compose_lts.Make (IntLang) (TypingLTS))
  | (true, false, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      (module Ogs.Compose_lts.Make (IntLang) (TypingLTS))
  | (false, true, CPS) ->
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Compose_lts.Make (IntLang) (TypingLTS))
  | (true, true, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Compose_lts.Make (IntLang) (TypingLTS))
  (* Direct style is intrinsically well-bracketed: well-bracketing needs no
     enforcement, and visibility is enforced by the stack-based LTS alone. *)
  | (true, false, DirectStyle) ->
      (module Ogs.Compose_lts.Make (IntLang) (TypingLTS))
  | (_, true, DirectStyle) ->
      let module TypingLTS = Ogs.Vis_lts.MakeStackBased (TypingLTS) in
      (module Ogs.Compose_lts.Make (IntLang) (TypingLTS))

let build_symbolic_lts kind : (module MULTI_RESULT_LTS_WITH_INIT) =
  let (module OpLang) = build_oplang_multi kind in
  let (module IntLang) = build_intlang_multi kind (module OpLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  match
    ( List.mem WellBracketing kind.restrictions,
      List.mem Visibility kind.restrictions,
      kind.control )
  with
  | (false, false, _) -> (module Ogs.Ogslts.MakeWithInit (IntLang) (TypingLTS))
  | (true, false, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      (module Ogs.Ogslts.MakeWithInit (IntLang) (TypingLTS))
  | (false, true, CPS) ->
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Ogslts.MakeWithInit (IntLang) (TypingLTS))
  | (true, true, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Ogslts.MakeWithInit (IntLang) (TypingLTS))
  (* Direct style is intrinsically well-bracketed: well-bracketing needs no
     enforcement, and visibility is enforced by the stack-based LTS alone. *)
  | (true, false, DirectStyle) ->
      (module Ogs.Ogslts.MakeWithInit (IntLang) (TypingLTS))
  | (_, true, DirectStyle) ->
      let module TypingLTS = Ogs.Vis_lts.MakeStackBased (TypingLTS) in
      (module Ogs.Ogslts.MakeWithInit (IntLang) (TypingLTS))
