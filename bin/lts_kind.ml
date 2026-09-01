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
  restrictions: restriction list
} [@@deriving yojson]

(* This is only needed because parametrized types in package constraints
   (i.e. mod constraints in package types (first class modules)) are not
   supported by OCaml (as of 5.4) *)
module type SINGLE_OPLANG  = Lang.Language.WITHAVAL_INOUT    with type 'a EvalMonad.r = 'a
module type MULTI_OPLANG  = Lang.Language.WITHAVAL_INOUT    with type 'a EvalMonad.r = 'a list

module type SINGLE_INTLANG = Lang.Interactive.LANG_WITH_INIT with type 'a EvalMonad.r = 'a
module type MULTI_INTLANG = Lang.Interactive.LANG_WITH_INIT with type 'a EvalMonad.r = 'a list

module type SINGLE_RESULT_LTS_WITH_INIT = Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a
module type MULTI_RESULT_LTS_WITH_INIT  = Lts.Strategy.LTS_WITH_INIT with type 'a EvalMonad.r = 'a list

module type SINGLE_RESULT_COMPOSITION_WITH_INIT =
  Ogs.Compose_lts.COMPOSITION_WITH_INIT with type 'a EvalMonad.r = 'a

let build_oplang kind : (module SINGLE_OPLANG) =
  match kind.oplang with
  | RefML -> (module Refml.RefML.WithAValConcrete (Util.Monad.ListB))

let build_oplang_multi kind : (module MULTI_OPLANG) =
  match kind.oplang with
  | RefML -> (module Refml.RefML.WithAValSymbolic (Util.Monad.ListB))

let build_intlang kind (module OpLang : SINGLE_OPLANG) :
    (module SINGLE_INTLANG) =
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

let build_concrete_lts kind : (module SINGLE_RESULT_LTS_WITH_INIT) =
  let (module OpLang) = build_oplang kind in
  let (module IntLang) = build_intlang kind (module OpLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  match
    ( List.mem WellBracketing kind.restrictions,
      List.mem Visibility kind.restrictions,
      kind.control )
  with
  | (false, false, _) -> (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (true, false, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (false, true, CPS) ->
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (true, true, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  (* Direct style is intrinsically well-bracketed: well-bracketing needs no
     enforcement, and visibility is enforced by the stack-based LTS alone. *)
  | (true, false, DirectStyle) -> (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (_, true, DirectStyle) ->
      let module TypingLTS = Ogs.Vis_lts.MakeStackBased (TypingLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))

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
  | (false, false, _) -> (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (true, false, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (false, true, CPS) ->
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (true, true, CPS) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      let module TypingLTS = Ogs.Vis_lts.MakeNameIndexed (TypingLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  (* Direct style is intrinsically well-bracketed: well-bracketing needs no
     enforcement, and visibility is enforced by the stack-based LTS alone. *)
  | (true, false, DirectStyle) -> (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (_, true, DirectStyle) ->
      let module TypingLTS = Ogs.Vis_lts.MakeStackBased (TypingLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
