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
  control: control_structure;
  restrictions: restriction list
} [@@deriving yojson]

let build_oplang kind : (module Lang.Language.WITHAVAL_INOUT) =
  match kind.oplang with
  | RefML -> (module Refml.RefML.WithAVal (Util.Monad.ListB))

let build_intlang kind (module OpLang : Lang.Language.WITHAVAL_INOUT) :
    (module Lang.Interactive.LANG_WITH_INIT) =
  match kind.control with
  | DirectStyle -> (module Lang.Direct.Make (OpLang))
  | CPS ->
      let module CpsLang = Lang.Cps.MakeComp (OpLang) () in
      (module Lang.Interactive.Make (CpsLang))

let build_lts kind : (module Lts.Strategy.LTS_WITH_INIT) =
  let (module OpLang) = build_oplang kind in
  let (module IntLang) = build_intlang kind (module OpLang) in
  let module TypingLTS = Ogs.Typing.Make (IntLang) in
  match
    ( List.mem WellBracketing kind.restrictions,
      List.mem Visibility kind.restrictions )
  with
  | (false, false) -> (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (true, false) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (false, true) ->
      let module VisLTS = Ogs.Vis_lts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (VisLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
  | (true, true) ->
      let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
      let module VisLTS = Ogs.Vis_lts.Make (TypingLTS.Moves) in
      let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (VisLTS) in
      (module Ogs.Ogslts.Make (IntLang) (TypingLTS))
