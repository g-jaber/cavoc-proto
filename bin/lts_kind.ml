
type oplang = RefML
type control_structure = DirectStyle | CPS
type restriction = Visibility | WellBracketing

type kind_lts = {
  oplang: oplang;
  control: control_structure;
  restrictions: restriction list
}

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

(* Cannot work until we get modular explicit in OCaml 
let rec build_typing_lst kind (module IntLang : Lang.Interactive.LANG_WITH_INIT)
    : (module Lts.Typing.LTS) =
  let rec aux restrictions (module TypingLTS : Lts.Typing.LTS) :
      (module Lts.Typing.LTS) =
    match restrictions with
    | [] -> (module TypingLTS)
    | WellBracketing :: restrictions' ->
        let module WBLTS = Ogs.Wblts.Make (TypingLTS.Moves) in
        let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (WBLTS) in
        aux restrictions' (module TypingLTS)
    | Visibility :: restrictions' ->
        let module VisLTS = Ogs.Vis_lts.Make (TypingLTS.Moves) in
        let module TypingLTS = Lts.Product_lts.Make (TypingLTS) (VisLTS) in
        aux restrictions' (module TypingLTS) in
  aux kind.restrictions (module Ogs.Typing.Make (IntLang))
  *)

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