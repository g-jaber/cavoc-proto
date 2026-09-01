(* =============================================
   LTS_CONFIG: LTS configuration from the scenario
   =============================================
   Reads the LTS options of the current scenario and builds a kind_lts
   record. The state of record is the JS scenario object
   (window.cavocScenario, front/scenario.js): the Options menu edits it in
   sandbox mode and the tutorial levels overwrite it, so nothing here reads
   the DOM.
*)

open Js_of_ocaml

let get_bool obj key =
  let v : bool Js.t Js.Optdef.t = Js.Unsafe.get obj (Js.string key) in
  Js.Optdef.case v (fun () -> false) Js.to_bool

let scenario_options () =
  let scenario : 'a Js.Optdef.t =
    Js.Unsafe.get Js.Unsafe.global (Js.string "cavocScenario") in
  Js.Optdef.case scenario
    (fun () -> None)
    (fun scenario ->
      let options : 'a Js.Optdef.t =
        Js.Unsafe.get scenario (Js.string "options") in
      Js.Optdef.case options (fun () -> None) (fun options -> Some options))

(* The mode of the current scenario: "single" unless the scenario object says
   otherwise. Evaluate_code dispatches on it — "compose" runs the open
   composition of the first two cards. *)
let scenario_mode () =
  let scenario : 'a Js.Optdef.t =
    Js.Unsafe.get Js.Unsafe.global (Js.string "cavocScenario") in
  Js.Optdef.case scenario
    (fun () -> "single")
    (fun scenario ->
      let mode : Js.js_string Js.t Js.Optdef.t =
        Js.Unsafe.get scenario (Js.string "mode") in
      Js.Optdef.case mode (fun () -> "single") Js.to_string)

let generate_kind_lts () =
  let open Lts_kind in
  let oplang = RefML in
  match scenario_options () with
  | None ->
      (* No scenario object on this page: everything defaults to off. *)
      { oplang; symbolic= false; control= CPS; restrictions= [] }
  | Some options ->
      let symbolic = get_bool options "symbolic" in
      let control =
        if get_bool options "directStyle" then DirectStyle else CPS in
      let restrictions =
        if get_bool options "wellBracketing" then [ WellBracketing ] else []
      in
      let restrictions =
        if get_bool options "visibility" then Visibility :: restrictions
        else restrictions in
      { oplang; symbolic; control; restrictions }
