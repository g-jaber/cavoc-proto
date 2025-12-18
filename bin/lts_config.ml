(* =============================================
   LTS_CONFIG: LTS configuration from UI inputs
   =============================================
   Constructs LTS configuration by reading form inputs:
   - generate_kind_lts: Reads checkboxes/radio buttons
     and builds a kind_lts record with:
     * Programming language (RefML)
     * Evaluation strategy (Direct or CPS)
     * Restrictions (Visibility, Well-Bracketing)
*)

open Js_of_ocaml

let generate_kind_lts () =
  let open Lts_kind in
  let oplang = RefML in
  let control =
    match Dom_html.getElementById_opt "direct-style-check" with
    | None -> CPS
    | Some checkbox_elem ->
        match Js.Opt.to_option (Dom_html.CoerceTo.input checkbox_elem) with
        | Some input ->
            if Js.to_bool input##.checked then DirectStyle else CPS
        | None -> CPS
  in
  let restrictions =
    let res_list = ref [] in
    
    (match Dom_html.getElementById_opt "wellbracketing-check" with
    | Some el -> 
        (match Js.Opt.to_option (Dom_html.CoerceTo.input el) with
         | Some input when Js.to_bool input##.checked -> 
              res_list := WellBracketing :: !res_list
         | _ -> ())
    | None -> ());
    
    (match Dom_html.getElementById_opt "visibility-check" with
    | Some el -> 
        (match Js.Opt.to_option (Dom_html.CoerceTo.input el) with
         | Some input when Js.to_bool input##.checked -> 
              res_list := Visibility :: !res_list
         | _ -> ())
    | None -> ());
    
    !res_list
  in
  {oplang; control; restrictions}
