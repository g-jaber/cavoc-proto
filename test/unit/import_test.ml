(* Unit tests of the imported-name seeding of get_typed_ienv
   (lib/RefML/declaration.ml, get_imported_name_env): a module may refer to
   the names declared by an imported signature, which become its Opponent
   names. This is what lets the client of an open composition call the
   interface provided by the other component. *)

module OpLang = Refml.RefML.WithAValConcrete (Util.Monad.ListB)
module Namectx = OpLang.Namectx

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    print_endline ("FAIL: " ^ name)
  end

let buf = Lexing.from_string

(* The shared interface, provided by the other component. *)
let shared_sig () = buf "val f : int -> int"

(* A client whose body refers to [f], which it does not implement. *)
let client_implem () = buf "let g x = f x"
let client_sig () = buf "val g : int -> int"

(* Without imports the client's reference to [f] is an unbound variable, which
   is what makes the seeding necessary. It cannot be asserted here: the type
   checker reports it through Util.Error.fail_error, which exits the process
   rather than raising. *)

let () =
  let (_ienv, _store, namectxP, namectxO) =
    OpLang.get_typed_ienv ~imports:(shared_sig ()) (client_implem ())
      (client_sig ()) in

  (* The import became an Opponent name, at level 0 of the O-context: the
     front door of the composition relies on this ordering to pair it with
     the provider's Proponent name. *)
  check "the import occupies the O-context"
    (List.length (Namectx.get_names namectxO) = 1);
  check "the import is named after its identifier"
    (List.for_all
       (fun nn -> Namectx.show_name_in namectxO nn = "f")
       (Namectx.get_names namectxO));

  (* The client still exports its own name, unaffected by the import. *)
  check "the client's export is a Proponent name"
    (List.length (Namectx.get_names namectxP) = 1);
  check "the export is named after its identifier"
    (List.for_all
       (fun nn -> Namectx.show_name_in namectxP nn = "g")
       (Namectx.get_names namectxP));

  (* Omitting ?imports must stay exactly the standalone case. *)
  let (_, _, standaloneP, standaloneO) =
    OpLang.get_typed_ienv (buf "let h x = x") (buf "val h : int -> int") in
  check "a standalone module has no Opponent names"
    (Namectx.get_names standaloneO = []);
  check "a standalone module still exports"
    (List.length (Namectx.get_names standaloneP) = 1);

  if !failures = 0 then print_endline "import: all tests passed"
  else begin
    Printf.eprintf "import: %d test(s) failed\n" !failures;
    exit 1
  end
