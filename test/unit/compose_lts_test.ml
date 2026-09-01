(* End-to-end test of the sequential open-composition front door
   (lib/ogs/compose_lts.ml): two modules, the first providing the shared
   interface and the second written against it, are assembled into a
   composite whose Opponent sees only the client's public signature. *)

module OpLang = Refml.RefML.WithAValConcrete (Util.Monad.ListB)
module CpsLang = Lang.Cps.MakeComp (OpLang) ()
module IntLang = Lang.Interactive.Make (CpsLang)
module TypingLTS = Ogs.Typing.Make (IntLang)
module Composition = Ogs.Compose_lts.Make (IntLang) (TypingLTS)
module Namectx = IntLang.IEnv.Renaming.Namectx

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    print_endline ("FAIL: " ^ name)
  end

let buf = Lexing.from_string

let provider_implem () = buf "let f x = x + 1"
let provider_sig () = buf "val f : int -> int"
let client_implem () = buf "let g x = f x"
let client_sig () = buf "val g : int -> int"

let names_of namectx =
  List.map (Namectx.show_name_in namectx) (Namectx.get_names namectx)

let () =
  let pconf =
    Composition.lexing_init_pconf ~provider_implem:(provider_implem ())
      ~provider_sig:(provider_sig ()) ~client_implem:(client_implem ())
      ~client_sig:(client_sig ()) ~imported_sig:(provider_sig ()) in
  let pos = Composition.get_passive_pos pconf in

  (* The Opponent of the composite sees the client's public signature and
     nothing else: the provider's interface is internal to the composition. *)
  check "the client's export is the composite's only Proponent name"
    (names_of (TypingLTS.get_namectxP pos) = [ "g" ]);
  check "the shared interface is not externally visible"
    (not (List.mem "f" (names_of (TypingLTS.get_namectxP pos))));
  check "the composite starts with no external Opponent name"
    (names_of (TypingLTS.get_namectxO pos) = []);

  (* The shared name was seeded into Shared_LR, pairing the provider's
     Proponent name with the client's Opponent name for it. *)
  check "Shared_LR holds the one provided name"
    (match Composition.passive_conf_to_yojson pconf with
    | `Assoc fields -> (
        match List.assoc_opt "syncState" fields with
        | Some (`Assoc tables) -> (
            match List.assoc_opt "sharedLeft" tables with
            | Some (`List [ `List [ _; _ ] ]) -> true
            | _ -> false)
        | _ -> false)
    | _ -> false);

  if !failures = 0 then print_endline "compose_lts: all tests passed"
  else begin
    Printf.eprintf "compose_lts: %d test(s) failed\n" !failures;
    exit 1
  end
