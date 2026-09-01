(* Unit tests for views represented by sparse, level-based thinnings. *)

module TestTypes = struct
  type t = string

  let to_yojson s = `String s
  let pp = Format.pp_print_string
end

module TestNames =
  Lang.Names.MakeInt (struct
      let is_callable = true
      let is_cname = false
    end)
    (struct
      let prefix = "n"
    end)
    ()

module TestNamectx = Lang.Typectx.Make_List (TestNames) (TestTypes)
module Weakening = Lang.Renaming.MakeWeak (TestNamectx)
module View = Lts.View.Make (Weakening)

let failures = ref 0

let check name condition =
  if not condition then begin
    incr failures;
    Printf.eprintf "FAILED: %s\n" name
  end

let make_ctx entries =
  List.fold_left
    (fun ctx (hint, typ) -> snd (TestNamectx.add_fresh ctx hint typ))
    TestNamectx.empty entries

let types ctx =
  List.map (TestNamectx.lookup_exn ctx) (TestNamectx.get_names ctx)

let raises_invalid_argument thunk =
  try
    ignore (thunk ());
    false
  with Invalid_argument _ -> true

let () =
  let ambient =
    make_ctx
      [ ("a", "A"); ("b", "B"); ("c", "C"); ("d", "D"); ("e", "E") ]
  in
  let sparse = View.of_support ambient [ 4; 0; 2 ] in
  check "a sparse view canonicalizes its support"
    (View.support sparse = [ 0; 2; 4 ]);
  check "a sparse view retains its ambient context"
    (View.ambient_ctx sparse = ambient);
  check "a sparse view has dense local levels"
    (TestNamectx.get_names (View.local_ctx sparse) = [ 0; 1; 2 ]);
  check "a sparse view selects the corresponding local types"
    (types (View.local_ctx sparse) = [ "A"; "C"; "E" ]);
  check "ambient names are reindexed to dense local levels"
    (List.map (View.to_local sparse) [ 0; 1; 2; 3; 4 ]
    = [ Some 0; None; Some 1; None; Some 2 ]);
  check "local names reindex back to their ambient levels"
    (List.map (View.to_ambient sparse) [ 0; 1; 2 ] = [ 0; 2; 4 ]);
  check "membership is exactly membership of the thinning image"
    (View.contains sparse 0 && View.contains sparse 2 && View.contains sparse 4
    && (not (View.contains sparse 1))
    && not (View.contains sparse 3));

  let full = View.full ambient in
  check "the full view contains every ambient name"
    (View.support full = TestNamectx.get_names ambient);
  check "the full view reindexes names identically"
    (List.for_all
       (fun name ->
         View.to_local full name = Some name
         && View.to_ambient full name = name)
       (TestNamectx.get_names ambient));
  check "a view has a printable representation"
    (String.length (Format.asprintf "%a" View.pp sparse) > 0);
  check "a view has a JSON representation" (View.to_yojson sparse <> `Null)

let () =
  let old_ambient = make_ctx [ ("a", "A"); ("b", "B"); ("c", "C") ] in
  let original = View.of_support old_ambient [ 0; 2 ] in
  let extended_ambient =
    make_ctx
      [ ("alpha", "A"); ("beta", "B"); ("gamma", "C"); ("delta", "D") ]
  in
  let rebased = View.rebase ~ambient:extended_ambient original in
  check "rebase accepts changed display hints and a compatible extension"
    (View.ambient_ctx rebased = extended_ambient);
  check "rebase preserves the selected ambient levels"
    (View.support rebased = [ 0; 2 ]);
  check "rebase preserves the local types and reindexing"
    (types (View.local_ctx rebased) = [ "A"; "C" ]
    && View.to_local rebased 0 = Some 0
    && View.to_local rebased 2 = Some 1
    && View.to_ambient rebased 1 = 2);

  let larger_ambient =
    make_ctx
      [
        ("A'", "A");
        ("B'", "B");
        ("C'", "C");
        ("D'", "D");
        ("x", "X");
        ("y", "Y");
        ("z", "Z");
      ]
  in
  let extended =
    View.extend ~ambient:larger_ambient ~fresh:[ 6; 4 ] rebased in
  check "extend accepts non-contiguous fresh ambient levels"
    (View.support extended = [ 0; 2; 4; 6 ]);
  check "non-contiguous fresh names receive dense local levels"
    (List.map (View.to_local extended) [ 0; 1; 2; 3; 4; 5; 6 ]
    = [ Some 0; None; Some 1; None; Some 2; None; Some 3 ]);
  check "extend preserves old and fresh selected types"
    (types (View.local_ctx extended) = [ "A"; "C"; "X"; "Z" ]);
  check "extend rejects a fresh name already present in the view"
    (raises_invalid_argument (fun () ->
         View.extend ~ambient:larger_ambient ~fresh:[ 2 ] rebased));
  check "extend rejects a fresh name outside the new ambient context"
    (raises_invalid_argument (fun () ->
         View.extend ~ambient:larger_ambient ~fresh:[ 7 ] rebased))

module LeftNames =
  Lang.Names.MakeInt (struct
      let is_callable = true
      let is_cname = false
    end)
    (struct
      let prefix = "f"
    end)
    ()

module RightNames =
  Lang.Names.MakeInt (struct
      let is_callable = false
      let is_cname = true
    end)
    (struct
      let prefix = "c"
    end)
    ()

module LeftNamectx = Lang.Typectx.Make_List (LeftNames) (TestTypes)
module RightNamectx = Lang.Typectx.Make_List (RightNames) (TestTypes)
module AggregateNames = Lang.Names.MakeAggregate (LeftNames) (RightNames)

module AggregateNamectx =
  Lang.Typectx.Aggregate (LeftNamectx) (RightNamectx) (AggregateNames)

module LeftWeakening = Lang.Renaming.MakeWeak (LeftNamectx)
module RightWeakening = Lang.Renaming.MakeWeak (RightNamectx)

module AggregateWeakening =
  Lang.Renaming.AggregateWeak
    (LeftWeakening)
    (RightWeakening)
    (AggregateNamectx)

module AggregateView = Lts.View.Make (AggregateWeakening)

let make_left_ctx entries =
  List.fold_left
    (fun ctx (hint, typ) -> snd (LeftNamectx.add_fresh ctx hint typ))
    LeftNamectx.empty entries

let make_right_ctx entries =
  List.fold_left
    (fun ctx (hint, typ) -> snd (RightNamectx.add_fresh ctx hint typ))
    RightNamectx.empty entries

let () =
  let left = make_left_ctx [ ("f0", "L0"); ("f1", "L1"); ("f2", "L2") ] in
  let right = make_right_ctx [ ("c0", "R0"); ("c1", "R1") ] in
  let aggregate =
    AggregateView.of_support (left, right)
      [ Either.Right 1; Either.Left 2; Either.Left 0 ] in
  check "aggregate support is canonicalized componentwise"
    (AggregateView.support aggregate
    = [ Either.Left 0; Either.Left 2; Either.Right 1 ]);
  check "aggregate local contexts are dense in each component"
    (let (local_left, local_right) = AggregateView.local_ctx aggregate in
     LeftNamectx.get_names local_left = [ 0; 1 ]
     && RightNamectx.get_names local_right = [ 0 ]);
  check "aggregate conversion preserves component tags"
    (AggregateView.to_local aggregate (Either.Left 0) = Some (Either.Left 0)
    && AggregateView.to_local aggregate (Either.Left 1) = None
    && AggregateView.to_local aggregate (Either.Left 2) = Some (Either.Left 1)
    && AggregateView.to_local aggregate (Either.Right 0) = None
    && AggregateView.to_local aggregate (Either.Right 1)
       = Some (Either.Right 0)
    && AggregateView.to_ambient aggregate (Either.Left 1) = Either.Left 2
    && AggregateView.to_ambient aggregate (Either.Right 0) = Either.Right 1);

  if !failures = 0 then print_endline "view: all tests passed"
  else begin
    Printf.eprintf "view: %d test(s) failed\n" !failures;
    exit 1
  end
