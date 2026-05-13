(* Note: it's not possible to use RefML.MakeComp because the MakeComp functor
  returns a module with signature Lang.Language.COMP and the Store module
  contained in Lang.Languages.COMP is not compatible with Refml.Store. 
  These two Store modules are actually the same but OCaml doesn't know
  about this. *)

open Refml

let () =
  Util.Debug.debug_mode := true ;

  let lexbuf = Lexing.from_channel stdin in

  let expr = RefML.parse_and_handle_error Parser.fullexpr lexbuf in

  let type_ctx = Type_ctx.build_type_ctx expr in
  let store    = Store.empty_store in

  let _, ty = Type_checker.typing_expr type_ctx expr in

  Format.printf "type: %a\n%!" Types.pp_typ ty ;
  
  let pp_opconf fmt (expr, store) =
    Format.fprintf fmt "<term: %a, store: %a>"
      Syntax.pp_value expr
      Store.pp_store store
  in
  Format.printf "opconf: %a\n%!" pp_opconf (expr, store) ;

  match Interpreter.normalize_opconf (expr, store) with
  | None -> Format.printf "opconf (after eval): the program diverges\n%!"
  | Some opconf -> Format.printf "opconf (after eval): %a\n%!" pp_opconf opconf
