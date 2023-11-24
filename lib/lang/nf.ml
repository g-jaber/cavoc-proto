(* Evaluation of programs may produces:
   - divergence
   - uncatchable error
   - raised exception that has not (yet) been caught
   - values
   - callbacks to function provided by another module
*)

type ('value, 'ectx, 'fname, 'cname) kind_nf =
  | NFCallback of 'fname * 'value * 'ectx
  | NFValue of 'cname * 'value
  | NFError of 'cname
  | NFRaise of 'cname * 'value

let is_error = function NFError _ -> true | _ -> false

let map_kind_nf empty_res concat f_val f_ectx = function
  | NFCallback (fn, value, ectx) ->
      let (value', res_v) = f_val value in
      let (ectx', res_k) = f_ectx ectx in
      (NFCallback (fn, value', ectx'), concat res_k res_v)
  | NFValue (cn, value) ->
      let (value', res) = f_val value in
      (NFValue (cn, value'), res)
  | NFError _ as k -> (k, empty_res)
  | NFRaise (cn, value) ->
      let (value', res) = f_val value in
      (NFRaise (cn, value'), res)

let string_of_kind_nf dir string_of_value string_of_ectx string_of_fname
    string_of_cname nf =
  let string_of_cname' cn =
    let cn_str = string_of_cname cn in
    if cn_str != "" && dir = "" then "[" ^ cn_str ^ "]" else cn_str in
  match nf with
  | NFCallback (fn, value, ectx) ->
      string_of_fname fn ^ dir ^ string_of_value value ^ string_of_ectx ectx
  | NFValue (cn, value) -> string_of_cname' cn ^ dir ^ string_of_value value
  | NFError cn -> string_of_cname' cn ^ dir ^ "error"
  | NFRaise (cn, value) ->
      string_of_cname' cn ^ dir ^ "raise " ^ string_of_value value

let get_active_name = function
  | NFCallback (fn, _, _) -> fn
  | NFValue (cn, _) -> cn
  | NFError cn -> cn
  | NFRaise (cn, _) -> cn

let equiv_kind_nf unify_abstract_val span anf1 anf2 =
  match (anf1, anf2) with
  | (NFCallback (fn1, aval1, _), NFCallback (fn2, aval2, _))
    when Util.Namespan.is_in_dom_im (fn1, fn2) span ->
      unify_abstract_val span aval1 aval2
  | (NFValue (cn1, aval1), NFValue (cn2, aval2)) when Util.Namespan.is_in_dom_im (cn1, cn2) span ->
      unify_abstract_val span aval1 aval2
  | (NFError cn1, NFError cn2) when Util.Namespan.is_in_dom_im (cn1, cn2) span -> Some span
  | (NFRaise (cn1, aval1), NFRaise (cn2, aval2)) when Util.Namespan.is_in_dom_im (cn1, cn2) span ->
      unify_abstract_val span aval1 aval2
  | _ -> None
