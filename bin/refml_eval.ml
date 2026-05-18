(* Note: it's not possible to use RefML.MakeComp because the MakeComp functor
  returns a module with signature Lang.Language.COMP and the Store module
  contained in Lang.Languages.COMP is not compatible with Refml.Store. 
  These two Store modules are actually the same but OCaml doesn't know
  about this.
  TODO: add symbolic execution related functions to the Language signatures... *)

open Refml

let symbolic_names =
  [ "x", Types.TBool
  ; "y", Types.TBool
  ; "z", Types.TBool
  ]

let () =
  Util.Debug.debug_mode := true ;

  let lexbuf = Lexing.from_channel stdin in

  let expr = RefML.parse_and_handle_error Parser.fullexpr lexbuf in

  let type_ctx = Type_ctx.build_type_ctx () in
  let store    = Store.empty_store in

  (* TODO: add symbolic_add to the store signature in lib/lang/language.mli ? *)
  let register_symbolic (store, tyctx) (name, ty) =
    let store = Store.symbolic_add_named store name ty in
    let tyctx = Type_ctx.extend_var_ctx tyctx name ty in

    (store, tyctx)
  in

  let store, type_ctx = List.fold_left
    register_symbolic (store, type_ctx) symbolic_names in

  let _, _ = Type_checker.typing_expr type_ctx expr in
  
  let pp_opconf fmt (expr, store) =
    Format.fprintf fmt "<term: %a, store: %a>"
      Syntax.pp_value expr
      Store.pp_store store
  in
  Format.printf "opconf: %a\n%!" pp_opconf (expr, store) ;

  let enumerate lst =
    let ids = List.init (List.length lst) (fun i -> i) in
    List.combine ids lst
  in

  match Interpreter.normalize_opconf (expr, store) with
  | [] -> Format.printf "opconf (after eval): the program diverges\n%!"
  | _ :: _ as opconfs ->
      let p (id, opconf) = Format.printf "opconf %d (after eval): %a\n%!" id pp_opconf opconf in
      List.iter p (enumerate opconfs)
