(* Evaluation of programs may produces:
   - divergence
   - uncatchable error
   - raised exception that has not (yet) been caught
   - values
   - callbacks to function provided by another module
*)

type ('value, 'ectx, 'fname, 'cname) nf_term =
  | NFCallback of 'fname * 'value * 'ectx
  | NFValue of 'cname * 'value
  | NFError of 'cname
  | NFRaise of 'cname * 'value 
  [@@deriving to_yojson]

let pp_nf_term ~pp_dir pp_val pp_ectx pp_fn pp_cn fmt = function
  | NFCallback (fn, value, ectx) ->
      let string_ectx = Format.asprintf "%a" pp_ectx ectx in
      let string_ectx' = if string_ectx = "" then "" else string_ectx in
      Format.fprintf fmt "%a%t(%a%s)" pp_fn fn pp_dir pp_val value string_ectx'
      (* TODO: Improve the code above *)
  | NFValue (cn, value) ->
      Format.fprintf fmt "%a%t%a" pp_cn cn pp_dir pp_val value
  | NFError cn -> Format.fprintf fmt "%a%t(error)" pp_cn cn pp_dir
  | NFRaise (cn, value) ->
      Format.fprintf fmt "%a%t(raise %a)" pp_cn cn pp_dir pp_val value

let string_of_nf_term dir f_val f_ectx f_fn f_cn = function
  | NFCallback (fn, value, ectx) ->
      let string_ectx = f_ectx ectx in
      let string_ectx' = if string_ectx = "" then "" else "," ^ string_ectx in
      f_fn fn ^ dir ^ "(" ^ f_val value ^ string_ectx' ^ ")"
  | NFValue (cn, value) -> f_cn cn ^ dir ^ "(" ^ f_val value ^ ")"
  | NFError cn -> f_cn cn ^ dir ^ "(error)"
  | NFRaise (cn, value) -> f_cn cn ^ dir ^ "(raise" ^ f_val value ^ ")"

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

let map ~f_cn ~f_fn ~f_val ~f_ectx = function
  | NFCallback (fn, value, ectx) ->
      NFCallback (f_fn fn, f_val value, f_ectx ectx)
  | NFValue (cn, value) -> NFValue (f_cn cn, f_val value)
  | NFError cn -> NFError (f_cn cn)
  | NFRaise (cn, value) -> NFRaise (f_cn cn, f_val value)

let map_val empty_res f_val = function
  | NFCallback (fn, value, ectx) ->
      let (value', res) = f_val value in
      (NFCallback (fn, value', ectx), res)
  | NFValue (cn, value) ->
      let (value', res) = f_val value in
      (NFValue (cn, value'), res)
  | NFError cn -> (NFError cn, empty_res)
  | NFRaise (cn, value) ->
      let (value', res) = f_val value in
      (NFRaise (cn, value'), res)

let map_ectx empty_res f_ectx = function
  | NFCallback (fn, value, ectx) ->
      let (ectx', res) = f_ectx ectx in
      (NFCallback (fn, value, ectx'), res)
  | NFValue (cn, value) -> (NFValue (cn, value), empty_res)
  | NFError cn -> (NFError cn, empty_res)
  | NFRaise (cn, value) -> (NFRaise (cn, value), empty_res)

let map_fn empty_res f_fn = function
  | NFCallback (fn, value, ectx) ->
      let (fn', res) = f_fn fn in
      (NFCallback (fn', value, ectx), res)
  | NFValue (cn, value) -> (NFValue (cn, value), empty_res)
  | NFError cn -> (NFError cn, empty_res)
  | NFRaise (cn, value) -> (NFRaise (cn, value), empty_res)

let map_cn empty_res f_cn = function
  | NFCallback (fn, value, ectx) -> (NFCallback (fn, value, ectx), empty_res)
  | NFValue (cn, value) ->
      let (cn', res) = f_cn cn in
      (NFValue (cn', value), res)
  | NFError cn ->
      let (cn', res) = f_cn cn in
      (NFError cn', res)
  | NFRaise (cn, value) ->
      let (cn', res) = f_cn cn in
      (NFRaise (cn', value), res)

let type_annotating_val ~inj_ty ~get_type_fname ~get_type_cname = function
  | NFCallback (fn, value, ectx) ->
      Util.Debug.print_debug "type annotating val callback";
      let ty_arg = get_type_fname fn in
      NFCallback (fn, (value, ty_arg), ectx)
  | NFValue (cn, value) ->
      Util.Debug.print_debug "type annotating val return";
      let ty = get_type_cname cn in
      NFValue (cn, (value, ty))
  | NFError _ as res -> res
  | NFRaise (cn, value) -> NFRaise (cn, (value, inj_ty Types.TExn))

let type_annotating_ectx ~get_type_fname ty_out = function
  | NFCallback (fn, value, ectx) ->
      let ty_hole = get_type_fname fn in
      NFCallback (fn, value, (ectx, (ty_hole, ty_out)))
  | NFValue (cn, value) -> NFValue (cn, value)
  | NFError _ as res -> res
  | NFRaise (cn, value) -> NFRaise (cn, value)

let type_check_nf_term ~inj_ty ~empty_res ~get_type_fname ~get_type_cname
    ~type_check_call ~type_check_ret = function
  | NFCallback (fn, value, _) ->
      let nty = get_type_fname fn in
      type_check_call value nty
  | NFValue (cn, value) ->
      let (ty_in, ty_out) = get_type_cname cn in
      type_check_ret value ty_in ty_out
  | NFError _ -> Some empty_res
  | NFRaise (cn, value) ->
      let (_, ty_out) = get_type_cname cn in
      let ty_in = inj_ty Types.TExn in
      type_check_ret value ty_in ty_out

module Make (BranchMonad : Util.Monad.BRANCH) = struct
  open BranchMonad

  (* the following function is in general called with cname=unit*)
  let generate_nf_term_call fname_ctx =
    let callback_l =
      List.map
        (fun (fn, (ty_in, ty_out)) -> (NFCallback (fn, ty_in, ()), ty_out))
        (Util.Pmap.to_list fname_ctx) in
    BranchMonad.para_list @@ callback_l

  let generate_nf_term_ret inj_ty cname_ctx =
    let return_l =
      List.map
        (fun (cn, (ty_in, ty_out)) -> (NFValue (cn, ty_in), ty_out))
        (Util.Pmap.to_list cname_ctx) in
    let exn_l =
      let exn_ctx =
        Util.Pmap.map_im
          (fun (_, ty_out) -> (inj_ty Types.TExn, ty_out))
          cname_ctx in
      List.map
        (fun (cn, (ty_in, ty_out)) -> (NFRaise (cn, ty_in), ty_out))
        (Util.Pmap.to_list exn_ctx) in
    BranchMonad.para_list @@ return_l @ exn_l

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
end

let merge_val_ectx ~f_ret ~f_call = function
  | NFCallback (fn, value, ectx) ->
      let value' = f_call (value, ectx) in
      NFCallback (fn, value', ())
  | NFValue (cn, value) -> NFValue (cn, f_ret value)
  | NFError cn -> NFError cn
  | NFRaise (cn, value) -> NFRaise (cn, f_ret value)

let apply_val error_res f = function
  | NFCallback (_, value, _) | NFValue (_, value) | NFRaise (_, value) ->
      f value
  | NFError _ -> error_res

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
