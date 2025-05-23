(* Evaluation of programs may produces:
   - divergence
   - uncatchable error
   - raised exception that has not (yet) been caught
   - values
   - callbacks to function provided by another module
*)

type ('value, 'ectx, 'fname, 'cname, 'opsym ) nf_term =
  | NFCallback of 'fname * 'value * 'ectx
  | NFValue of 'cname * 'value
  | NFError of 'cname
  | NFRaise of 'cname * 'value
  | NFPerform of 'cname * 'opsym * 'value * 'ectx

let is_error = function NFError _ -> true | _ -> false

let cps_nf_term cn f_val f_valctx = function
  | NFCallback (fn, value, ectx) ->
      let value' = f_valctx (value, cn, ectx) in
      NFCallback (fn, value', ())
  | NFValue (_, value) ->
      let value' = f_val value in
      NFValue (cn, value')
  | NFError _ -> NFError cn
  | NFRaise (_, value) ->
      let value' = f_val value in
      NFRaise (cn, value')
  | NFPerform _ -> failwith "NFPerform not yet implemented"

let map ~f_cn ~f_fn ~f_val ~f_ectx = function
  | NFCallback (fn, value, ectx) ->
      NFCallback (f_fn fn, f_val value, f_ectx ectx)
  | NFValue (cn, value) -> NFValue (f_cn cn, f_val value)
  | NFError cn -> NFError (f_cn cn)
  | NFRaise (cn, value) -> NFRaise (f_cn cn, f_val value)
  | NFPerform _ -> failwith "NFPerform not yet implemented"

module Make (M : Util.Monad.BRANCH) = struct
  open M

  let generate_nf_term_call cname_ctx =
    let callback_l =
      List.map
        (fun (fn, (value, ectx)) -> NFCallback (fn, value, ectx))
        (Util.Pmap.to_list fname_ctx) in
    M.para_list @@ callback_l

  let generate_nf_term_ret fname_ctx =
    let return_l =
      List.map
        (fun (cn, value) -> NFValue (cn, value))
        (Util.Pmap.to_list cname_ctx) in
    let exn_l =
      let exn_ctx = Util.Pmap.map_im (fun _ -> Types.exception_type) cname_ctx in
      List.map
        (fun (cn, value) -> NFRaise (cn, value))
        (Util.Pmap.to_list exn_ctx) in
    M.para_list @ return_l @ exn_l

  let abstract_nf_term_m ~gen_val = function
    | NFCallback (fn, value, _) ->
        let* (value', res) = gen_val value in
        return (NFCallback (fn, value', ()), res)
    | NFValue (cn, value) ->
        let* (value', res) = gen_val value in
        return (NFValue (cn, value'), res)
    | NFError _ -> fail ()
    | NFRaise (cn, value) ->
        let* (value', res) = gen_val value in
        return (NFRaise (cn, value'), res)
    | NFPerform _ -> failwith "NFPerform not yet implemented"
end

let merge_val_ectx ~f_ret ~f_call = function
  | NFCallback (fn, value, ectx) ->
      let value' = f_call (value, ectx) in
      NFCallback (fn, value', ())
  | NFValue (cn, value) -> NFValue (cn, f_ret value)
  | NFError cn -> NFError cn
  | NFRaise (cn, value) -> NFRaise (cn, f_ret value)
  | NFPerform _ -> failwith "NFPerform not yet implemented"

let apply_val error_res f = function
  | NFCallback (_, value, _) | NFValue (_, value) | NFRaise (_, value) ->
      f value
  | NFError _ -> error_res
  | NFPerform _ -> failwith "NFPerform not yet implemented"


let equiv_nf_term unify_abstract_val span anf1 anf2 =
  match (anf1, anf2) with
  | (NFCallback (fn1, aval1, _), NFCallback (fn2, aval2, _))
    when Util.Namespan.is_in_dom_im (fn1, fn2) span ->
      unify_abstract_val span aval1 aval2
  | (NFValue (cn1, aval1), NFValue (cn2, aval2))
    when Util.Namespan.is_in_dom_im (cn1, cn2) span ->
      unify_abstract_val span aval1 aval2
  | (NFError cn1, NFError cn2) when Util.Namespan.is_in_dom_im (cn1, cn2) span
    ->
      Some span
  | (NFRaise (cn1, aval1), NFRaise (cn2, aval2))
    when Util.Namespan.is_in_dom_im (cn1, cn2) span ->
      unify_abstract_val span aval1 aval2
  | _ -> None
