(* Unit tests for sparse, level-based thinnings and their componentwise
   aggregate.  The tests exercise the algebra independently of moves/views so
   that failures identify the context-map layer itself. *)

module TestTypes = struct
  type t = string

  let to_yojson s = `String s
  let pp = Format.pp_print_string
end

module TestNames =
  Lang.Names.MakeInt (struct
      let is_callable = true let is_cname = false
    end)
    (struct
      let prefix = "n"
    end)
    ()

module TestNamectx = Lang.Typectx.Make_List (TestNames) (TestTypes)
module Weak = Lang.Renaming.MakeWeak (TestNamectx)
module Thin = Lang.Renaming.MakeThin (Weak)

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

let string_of_support support =
  String.concat "," (List.map string_of_int support)

let strictly_increasing = function
  | [] -> true
  | first :: rest ->
      snd
        (List.fold_left
           (fun (previous, increasing) current ->
             (current, increasing && previous < current))
           (first, true) rest)

let raises_invalid_argument thunk =
  try
    ignore (thunk ());
    false
  with Invalid_argument _ -> true

let same_thinning thinning1 thinning2 =
  types (Thin.dom thinning1) = types (Thin.dom thinning2)
  && types (Thin.im thinning1) = types (Thin.im thinning2)
  && List.for_all
       (fun local ->
         Thin.to_ambient thinning1 local = Thin.to_ambient thinning2 local)
       (TestNamectx.get_names (Thin.dom thinning1))
  && List.for_all
       (fun ambient ->
         Thin.to_local thinning1 ambient = Thin.to_local thinning2 ambient)
       (TestNamectx.get_names (Thin.im thinning1))

let rec subsets = function
  | [] -> [ [] ]
  | name :: names ->
      let rest = subsets names in
      rest @ List.map (fun support -> name :: support) rest

let gamma = make_ctx [ ("", "A"); ("b", "B"); ("", "C"); ("d", "D"); ("", "E") ]

let () =
  (* [of_support] treats its argument as a set and gives its domain dense
     levels in ambient order. *)
  let sparse = Thin.of_support gamma [ 4; 0; 2 ] in
  check "sparse thinning retains the ambient context" (Thin.im sparse = gamma);
  check "sparse thinning has a dense three-name domain"
    (TestNamectx.get_names (Thin.dom sparse) = [ 0; 1; 2 ]);
  check "sparse thinning selects types in ambient order"
    (types (Thin.dom sparse) = [ "A"; "C"; "E" ]);
  check "sparse thinning maps its dense levels to the support"
    (List.map (Thin.to_ambient sparse) [ 0; 1; 2 ] = [ 0; 2; 4 ]);
  check "to_ambient aliases lookup"
    (List.for_all
       (fun local -> Thin.to_ambient sparse local = Thin.lookup sparse local)
       [ 0; 1; 2 ]);
  check "to_local is the partial inverse on a sparse image"
    (List.map (Thin.to_local sparse) [ 0; 1; 2; 3; 4 ]
    = [ Some 0; None; Some 1; None; Some 2 ]);
  check "is_in_dom sees exactly the dense domain"
    (Thin.is_in_dom sparse 0 && Thin.is_in_dom sparse 2
    && (not (Thin.is_in_dom sparse 3))
    && not (Thin.is_in_dom sparse (-1)));
  check "support order is ignored"
    (same_thinning sparse (Thin.of_support gamma [ 0; 2; 4 ]));

  (* Exhaust all supports of this small context.  This checks density, order,
     type preservation and both inverse directions without another test
     dependency. *)
  List.iter
    (fun support ->
      let label = "support [" ^ string_of_support support ^ "]" in
      let thinning = Thin.of_support gamma (List.rev support) in
      let locals = TestNamectx.get_names (Thin.dom thinning) in
      let images = List.map (Thin.to_ambient thinning) locals in
      check (label ^ " is canonicalized") (images = support);
      check
        (label ^ " has dense local levels")
        (locals = List.init (List.length support) Fun.id);
      check (label ^ " is order-preserving") (strictly_increasing images);
      check
        (label ^ " preserves types")
        (List.for_all
           (fun local ->
             TestNamectx.lookup_exn (Thin.dom thinning) local
             = TestNamectx.lookup_exn (Thin.im thinning)
                 (Thin.to_ambient thinning local))
           locals);
      check
        (label ^ " has a left inverse")
        (List.for_all
           (fun local ->
             Thin.to_local thinning (Thin.to_ambient thinning local)
             = Some local)
           locals);
      check
        (label ^ " has the expected partial inverse")
        (List.for_all
           (fun ambient ->
             match Thin.to_local thinning ambient with
             | None -> not (List.mem ambient support)
             | Some local ->
                 List.mem ambient support
                 && Thin.to_ambient thinning local = ambient)
           (TestNamectx.get_names gamma)))
    (subsets (TestNamectx.get_names gamma));

  check "empty support has an empty domain"
    (TestNamectx.is_empty (Thin.dom (Thin.of_support gamma [])));
  check "empty ambient context supports the empty thinning"
    (let thinning = Thin.of_support TestNamectx.empty [] in
     TestNamectx.is_empty (Thin.dom thinning)
     && TestNamectx.is_empty (Thin.im thinning));
  check "duplicate support is rejected"
    (raises_invalid_argument (fun () -> Thin.of_support gamma [ 0; 0 ]));
  check "negative support level is rejected"
    (raises_invalid_argument (fun () -> Thin.of_support gamma [ -1 ]));
  check "out-of-domain support level is rejected"
    (raises_invalid_argument (fun () -> Thin.of_support gamma [ 5 ]));

  (* Composition follows the existing convention: [compose outer inner] is
     outer after inner. *)
  let outer = Thin.of_support gamma [ 0; 2; 4 ] in
  let inner = Thin.of_support (Thin.dom outer) [ 1; 2 ] in
  let composite = Thin.compose outer inner in
  check "composition has the inner domain"
    (types (Thin.dom composite) = [ "C"; "E" ]);
  check "composition has the outer ambient context" (Thin.im composite = gamma);
  check "composition maps through both sparse supports"
    (List.map (Thin.to_ambient composite) [ 0; 1 ] = [ 2; 4 ]);
  check "composition computes the corresponding partial inverse"
    (List.map (Thin.to_local composite) [ 0; 1; 2; 3; 4 ]
    = [ None; None; Some 0; None; Some 1 ]);
  let rebuilt_outer_domain = make_ctx [ ("c", "C"); ("e", "E") ] in
  let rebuilt_inner = Thin.of_support rebuilt_outer_domain [ 0; 1 ] in
  check "composition ignores display hints in compatible contexts"
    (List.map (Thin.to_ambient (Thin.compose composite rebuilt_inner)) [ 0; 1 ]
    = [ 2; 4 ]);
  check "left identity law"
    (same_thinning (Thin.compose (Thin.id (Thin.im outer)) outer) outer);
  check "right identity law"
    (same_thinning (Thin.compose outer (Thin.id (Thin.dom outer))) outer);
  check "sparse thinning printer produces output"
    (String.length (Thin.to_string sparse) > 0)

let check_weakening_directions label lookup to_ambient to_local weak_l weak_r =
  let delta = make_ctx [ ("", "X"); ("", "Y") ] in
  let other = make_ctx [ ("", "U"); ("", "V") ] in
  let left = weak_l delta other in
  let right = weak_r delta other in
  check
    (label ^ " weak_l maps the left block")
    (List.map (to_ambient left) [ 0; 1 ] = [ 0; 1 ]);
  check
    (label ^ " weak_l excludes the appended right block")
    (List.map (to_local left) [ 0; 1; 2; 3 ] = [ Some 0; Some 1; None; None ]);
  check
    (label ^ " weak_r maps past the prefix")
    (List.map (to_ambient right) [ 0; 1 ] = [ 2; 3 ]);
  check
    (label ^ " weak_r excludes the prefix")
    (List.map (to_local right) [ 0; 1; 2; 3 ] = [ None; None; Some 0; Some 1 ]);
  check
    (label ^ " forward operation aliases lookup")
    (List.for_all
       (fun local ->
         to_ambient left local = lookup left local
         && to_ambient right local = lookup right local)
       [ 0; 1 ])

let () =
  check_weakening_directions "MakeThin" Thin.lookup Thin.to_ambient
    Thin.to_local Thin.weak_l Thin.weak_r;
  check_weakening_directions "MakeWeak" Weak.lookup Weak.to_ambient
    Weak.to_local Weak.weak_l Weak.weak_r

module LeftNames =
  Lang.Names.MakeInt (struct
      let is_callable = true let is_cname = false
    end)
    (struct
      let prefix = "l"
    end)
    ()

module RightNames =
  Lang.Names.MakeInt (struct
      let is_callable = false let is_cname = true
    end)
    (struct
      let prefix = "r"
    end)
    ()

module LeftNamectx = Lang.Typectx.Make_List (LeftNames) (TestTypes)
module RightNamectx = Lang.Typectx.Make_List (RightNames) (TestTypes)
module AggregateNames = Lang.Names.MakeAggregate (LeftNames) (RightNames)

module AggregateNamectx =
  Lang.Typectx.Aggregate (LeftNamectx) (RightNamectx) (AggregateNames)

module LeftWeak = Lang.Renaming.MakeWeak (LeftNamectx)
module RightWeak = Lang.Renaming.MakeWeak (RightNamectx)

module AggregateWeak =
  Lang.Renaming.AggregateWeak (LeftWeak) (RightWeak) (AggregateNamectx)

module AggregateThin = Lang.Renaming.MakeThin (AggregateWeak)

module AggregateContiguousThin =
  Lang.Renaming.AggregateThin (LeftWeak) (RightWeak) (AggregateNamectx)

module OuterNames = Lang.Names.MakeAggregate (AggregateNames) (RightNames)

module OuterNamectx =
  Lang.Typectx.Aggregate (AggregateNamectx) (RightNamectx) (OuterNames)

module OuterWeak =
  Lang.Renaming.AggregateWeak (AggregateWeak) (RightWeak) (OuterNamectx)

module OuterThin = Lang.Renaming.MakeThin (OuterWeak)

let make_left_ctx entries =
  List.fold_left
    (fun ctx (hint, typ) -> snd (LeftNamectx.add_fresh ctx hint typ))
    LeftNamectx.empty entries

let make_right_ctx entries =
  List.fold_left
    (fun ctx (hint, typ) -> snd (RightNamectx.add_fresh ctx hint typ))
    RightNamectx.empty entries

let left_types ctx =
  List.map (LeftNamectx.lookup_exn ctx) (LeftNamectx.get_names ctx)

let right_types ctx =
  List.map (RightNamectx.lookup_exn ctx) (RightNamectx.get_names ctx)

let () =
  let left_gamma = make_left_ctx [ ("", "L0"); ("left", "L1"); ("", "L2") ] in
  let right_gamma = make_right_ctx [ ("", "R0"); ("right", "R1") ] in
  let ambient = (left_gamma, right_gamma) in
  let thinning =
    AggregateThin.of_support ambient
      [ Either.Right 1; Either.Left 2; Either.Left 0 ] in
  let (local_left, local_right) = AggregateThin.dom thinning in
  check "aggregate support makes dense component domains"
    (LeftNamectx.get_names local_left = [ 0; 1 ]
    && RightNamectx.get_names local_right = [ 0 ]);
  check "aggregate support preserves selected component types"
    (left_types local_left = [ "L0"; "L2" ]
    && right_types local_right = [ "R1" ]);
  check "aggregate support canonicalizes within each component"
    (AggregateThin.to_ambient thinning (Either.Left 0) = Either.Left 0
    && AggregateThin.to_ambient thinning (Either.Left 1) = Either.Left 2
    && AggregateThin.to_ambient thinning (Either.Right 0) = Either.Right 1);
  check "aggregate forward operation aliases lookup"
    (List.for_all
       (fun local ->
         AggregateThin.to_ambient thinning local
         = AggregateThin.lookup thinning local)
       (AggregateNamectx.get_names (AggregateThin.dom thinning)));
  check "aggregate partial inverse preserves tags"
    (AggregateThin.to_local thinning (Either.Left 0) = Some (Either.Left 0)
    && AggregateThin.to_local thinning (Either.Left 1) = None
    && AggregateThin.to_local thinning (Either.Left 2) = Some (Either.Left 1)
    && AggregateThin.to_local thinning (Either.Right 0) = None
    && AggregateThin.to_local thinning (Either.Right 1) = Some (Either.Right 0)
    );
  check "aggregate thinning preserves types"
    (List.for_all
       (fun local ->
         AggregateNamectx.lookup_exn (AggregateThin.dom thinning) local
         = AggregateNamectx.lookup_exn
             (AggregateThin.im thinning)
             (AggregateThin.to_ambient thinning local))
       (AggregateNamectx.get_names (AggregateThin.dom thinning)));
  check "aggregate support rejects a duplicate in one component"
    (raises_invalid_argument (fun () ->
         AggregateThin.of_support ambient [ Either.Left 0; Either.Left 0 ]));
  check "aggregate support rejects a name outside one component"
    (raises_invalid_argument (fun () ->
         AggregateThin.of_support ambient [ Either.Right 2 ]));
  check "equal levels in different components are distinct support names"
    (let both =
       AggregateThin.of_support ambient [ Either.Right 0; Either.Left 0 ] in
     AggregateNamectx.get_names (AggregateThin.dom both)
     = [ Either.Left 0; Either.Right 0 ]);

  let inner =
    AggregateThin.of_support
      (AggregateThin.dom thinning)
      [ Either.Right 0; Either.Left 1 ] in
  let composite = AggregateThin.compose thinning inner in
  check "aggregate composition maps componentwise"
    (AggregateThin.to_ambient composite (Either.Left 0) = Either.Left 2
    && AggregateThin.to_ambient composite (Either.Right 0) = Either.Right 1);
  check "aggregate composition has a componentwise partial inverse"
    (AggregateThin.to_local composite (Either.Left 2) = Some (Either.Left 0)
    && AggregateThin.to_local composite (Either.Right 1) = Some (Either.Right 0)
    && AggregateThin.to_local composite (Either.Left 0) = None);

  let delta =
    (make_left_ctx [ ("", "DL") ], make_right_ctx [ ("", "DR0"); ("", "DR1") ])
  in
  let prefix =
    (make_left_ctx [ ("", "PL0"); ("", "PL1") ], make_right_ctx [ ("", "PR0") ])
  in
  let weak_right = AggregateThin.weak_r delta prefix in
  check "aggregate weak_r uses the left-component offset"
    (AggregateThin.to_ambient weak_right (Either.Left 0) = Either.Left 2);
  check "aggregate weak_r uses the independent right-component offset"
    (AggregateThin.to_ambient weak_right (Either.Right 0) = Either.Right 1
    && AggregateThin.to_ambient weak_right (Either.Right 1) = Either.Right 2);
  check "aggregate weak_r partial inverse excludes both prefixes"
    (AggregateThin.to_local weak_right (Either.Left 0) = None
    && AggregateThin.to_local weak_right (Either.Left 1) = None
    && AggregateThin.to_local weak_right (Either.Left 2) = Some (Either.Left 0)
    && AggregateThin.to_local weak_right (Either.Right 0) = None
    && AggregateThin.to_local weak_right (Either.Right 1)
       = Some (Either.Right 0)
    && AggregateThin.to_local weak_right (Either.Right 2)
       = Some (Either.Right 1));
  let contiguous = AggregateContiguousThin.weak_r delta prefix in
  check "plain aggregate thinning accepts contiguous component thinnings"
    (AggregateContiguousThin.to_ambient contiguous (Either.Left 0)
     = Either.Left 2
    && AggregateContiguousThin.to_local contiguous (Either.Right 0) = None
    && AggregateContiguousThin.to_local contiguous (Either.Right 2)
       = Some (Either.Right 1));

  let outer_right = make_right_ctx [ ("", "O0"); ("", "O1") ] in
  let outer_ambient = (ambient, outer_right) in
  let outer =
    OuterThin.of_support outer_ambient
      [
        Either.Right 1;
        Either.Left (Either.Right 1);
        Either.Left (Either.Left 2);
      ] in
  check "generic sparse thinning supports nested aggregate contexts"
    (OuterThin.to_ambient outer (Either.Left (Either.Left 0))
     = Either.Left (Either.Left 2)
    && OuterThin.to_ambient outer (Either.Left (Either.Right 0))
       = Either.Left (Either.Right 1)
    && OuterThin.to_ambient outer (Either.Right 0) = Either.Right 1);
  check "nested aggregate partial inverse preserves every tag"
    (OuterThin.to_local outer (Either.Left (Either.Left 2))
     = Some (Either.Left (Either.Left 0))
    && OuterThin.to_local outer (Either.Left (Either.Right 0)) = None
    && OuterThin.to_local outer (Either.Right 1) = Some (Either.Right 0));
  let rebuilt_outer_domain =
    ( ( make_left_ctx [ ("renamed-left", "L2") ],
        make_right_ctx [ ("renamed-inner-right", "R1") ] ),
      make_right_ctx [ ("renamed-outer-right", "O1") ] ) in
  let rebuilt_identity =
    OuterThin.of_support rebuilt_outer_domain
      [
        Either.Left (Either.Left 0);
        Either.Left (Either.Right 0);
        Either.Right 0;
      ] in
  let recomposed = OuterThin.compose outer rebuilt_identity in
  check "nested composition ignores display hints"
    (OuterThin.to_ambient recomposed (Either.Left (Either.Left 0))
     = Either.Left (Either.Left 2)
    && OuterThin.to_ambient recomposed (Either.Left (Either.Right 0))
       = Either.Left (Either.Right 1)
    && OuterThin.to_ambient recomposed (Either.Right 0) = Either.Right 1);

  if !failures = 0 then print_endline "thinning: all tests passed"
  else begin
    Printf.eprintf "thinning: %d test(s) failed\n" !failures;
    exit 1
  end
