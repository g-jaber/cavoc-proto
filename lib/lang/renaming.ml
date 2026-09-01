(* Weakenings are the single-block inclusions Δ ↪ Γ₁ + Δ + Γ₂,
   the composites of the coprojections of contexts. *)
module type WEAKENING = sig
  module Namectx : Typectx.TYPECTX

  type t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val id : Namectx.t -> t (* id Γ :  Γ → Γ *)
  val dom : t -> Namectx.t
  val im : t -> Namectx.t
  val compose : t -> t -> t

  (* weak_l Δ Γ : Δ → Δ + Γ*)
  val weak_l : Namectx.t -> Namectx.t -> t

  (* weak_r Δ Γ : Δ → Γ + Δ *)
  val weak_r : Namectx.t -> Namectx.t -> t
  val is_in_dom : t -> Namectx.Names.name -> bool
  val lookup : t -> Namectx.Names.name -> Namectx.Names.name
end

(* Thinnings are injective, order-preserving maps between contexts. *)
module type THINNING = sig
  include WEAKENING

  (* [lookup_inv thinning target] is the partial inverse of [lookup].  It
     returns [None] exactly when [target] is not in the image of [thinning].
     lookup_inv thinning (lookup thinning source) = Some source
     and [lookup_inv thinning target = Some source] implies
     [lookup thinning source = target]. *)
  val lookup_inv : t -> Namectx.Names.name -> Namectx.Names.name option
end

(* Renamings are general maps between contexts, extending weakenings with
   the copairing of the coproduct and the symmetry. 
   A general renaming may identify two names and
   therefore need not have a partial inverse. *)
module type RENAMING = sig
  include WEAKENING

  val copairing : t -> t -> t

  (* sym Δ Γ : Δ + Γ → Γ + Δ*)
  val sym : Namectx.t -> Namectx.t -> t
  val add_fresh : t -> string -> Namectx.typ -> Namectx.Names.name * t
  (* The second argument is used to associate a string to the fresh variable *)
end

module type WEAKENING_LIST = sig
  include WEAKENING with type Namectx.Names.name = int end

module type THINNING_LIST = sig
  include THINNING with type Namectx.Names.name = int end

module MakePmap (Namectx : Typectx.TYPECTX) :
  RENAMING with module Namectx = Namectx = struct
  module Namectx = Namectx

  type t = {
    map: (Namectx.Names.name, Namectx.Names.name) Util.Pmap.pmap;
    dom: Namectx.t;
    im: Namectx.t;
  }

  let pp_map fmt map =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, value) =
      Format.fprintf fmt "%a ↦ %a" Namectx.Names.pp_name n Namectx.Names.pp_name
        value in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt map

  let pp fmt renam =
    Format.fprintf fmt "%a : %a ⇒ %a" pp_map renam.map Namectx.pp renam.dom
      Namectx.pp renam.im

  let to_string = Format.asprintf "%a" pp

  let id namectx =
    let names_l = Namectx.get_names namectx in
    let map = Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, nn)) names_l in
    { map; dom= namectx; im= namectx }

  let dom renam = renam.dom
  let im renam = renam.im

  let compose renam1 renam2 =
    assert (renam1.dom = renam2.im);
    let dom = renam2.dom in
    let im = renam1.im in
    let map =
      Util.Pmap.map_im (fun nn -> Util.Pmap.lookup_exn nn renam1.map) renam2.map
    in
    { map; dom; im }

  let copairing renam1 renam2 =
    assert (renam1.im = renam2.im);
    let dom = Namectx.concat renam1.dom renam2.dom in
    let map = Util.Pmap.concat renam1.map renam2.map in
    { map; dom; im= renam1.im }

  let weak_l namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Namectx.concat namectx_l namectx_r }

  (* weak_r Δ Γ : Δ → Γ + Δ *)
  let weak_r namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Namectx.concat namectx_r namectx_l }

  let sym namectx_l namectx_r =
    let renam = id (Namectx.concat namectx_l namectx_r) in
    { renam with im= Namectx.concat namectx_r namectx_l }

  let is_in_dom renam nn = Util.Pmap.mem nn renam.map
  let lookup renam nn = Util.Pmap.lookup_exn nn renam.map

  let add_fresh (renam : t) (_str : string) (typ : Namectx.typ) :
      Namectx.Names.name * t =
    let (nn, lnamectx) = Namectx.singleton typ in
    let renam_nn = weak_r lnamectx renam.im in
    let renam' = { renam with im= Namectx.concat renam.im lnamectx } in
    let nn' = lookup renam_nn nn in
    (nn', copairing renam' renam_nn)
end

module Make (Namectx : Typectx.TYPECTX_LIST) :
  RENAMING with module Namectx = Namectx = struct
  module Namectx = Namectx

  type t = {
    map: (Namectx.Names.name, Namectx.Names.name) Util.Pmap.pmap;
    dom: Namectx.t;
    im: Namectx.t;
  }

  let pp_map fmt map =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, value) =
      Format.fprintf fmt "%a ↦ %a" Namectx.Names.pp_name n Namectx.Names.pp_name
        value in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt map

  let pp fmt renam =
    if Namectx.is_empty renam.dom && Namectx.is_empty renam.im then
      Format.fprintf fmt ""
    else
      Format.fprintf fmt "%a : [%a] ⇒ [%a]" pp_map renam.map Namectx.pp
        renam.dom Namectx.pp renam.im

  let to_string = Format.asprintf "%a" pp

  let id namectx =
    let names_l = Namectx.get_names namectx in
    let map = Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, nn)) names_l in
    { map; dom= namectx; im= namectx }

  let dom renam = renam.dom
  let im renam = renam.im

  let compose renam1 renam2 =
    assert (renam1.dom = renam2.im);
    let dom = renam2.dom in
    let im = renam1.im in
    let map =
      Util.Pmap.map_im (fun nn -> Util.Pmap.lookup_exn nn renam1.map) renam2.map
    in
    { map; dom; im }

  let copairing renam1 renam2 =
    assert (renam1.im = renam2.im);
    let dom = Namectx.concat renam1.dom renam2.dom in
    let map = Util.Pmap.concat renam1.map renam2.map in
    { map; dom; im= renam1.im }

  let weak_l namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Namectx.concat namectx_l namectx_r }

  (* weak_r Δ Γ : Δ → Γ + Δ*)
  let weak_r namectx_l namectx_r =
    let names_l = Namectx.get_names namectx_l in
    let offset = List.length @@ Util.Pmap.to_list @@ Namectx.to_pmap namectx_r in
    Util.Debug.print_debug @@ "Calling weak_r with an offset of "
    ^ string_of_int offset ^ " on the context "
    ^ Namectx.to_string namectx_l;
    let map =
      Util.Pmap.list_to_pmap @@ List.map (fun i -> (i, i + offset)) names_l
    in
    { map; dom= namectx_l; im= Namectx.concat namectx_r namectx_l }

  let sym namectx_l namectx_r =
    let size_l = List.length @@ Namectx.get_names namectx_l in
    let size_r = List.length @@ Namectx.get_names namectx_r in
    let map =
      Util.Pmap.list_to_pmap
      @@ List.init (size_l + size_r) (fun i ->
          if i < size_l then (i, size_r + i) else (i, i - size_l)) in
    {
      map;
      dom= Namectx.concat namectx_l namectx_r;
      im= Namectx.concat namectx_r namectx_l;
    }

  let is_in_dom renam nn = Util.Pmap.mem nn renam.map

  let lookup renam nn =
    try Util.Pmap.lookup_exn nn renam.map
    with Not_found ->
      Util.Debug.print_debug @@ "The name "
      ^ Namectx.Names.string_of_name nn
      ^ " was not found in the renaming " ^ to_string renam;
      raise Not_found

  let add_fresh (renam : t) (_str : string) (typ : Namectx.typ) :
      Namectx.Names.name * t =
    let (nn, lnamectx) = Namectx.singleton typ in
    let renam_nn = weak_r lnamectx renam.im in
    let renam' = { renam with im= Namectx.concat renam.im lnamectx } in
    let nn' = lookup renam_nn nn in
    (nn', copairing renam' renam_nn)
end

module MakeThin (Weakening : WEAKENING) : sig
  include THINNING with module Namectx = Weakening.Namectx

  val of_support : Namectx.t -> Namectx.Names.name list -> t
end = struct
  module Namectx = Weakening.Namectx

  type t = {
    map: (Namectx.Names.name, Namectx.Names.name) Util.Pmap.pmap;
    dom: Namectx.t;
    im: Namectx.t;
  }

  let pp_map fmt map =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (local, context_name) =
      Format.fprintf fmt "%a ↦ %a" Namectx.Names.pp_name local
        Namectx.Names.pp_name context_name in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt map

  let pp fmt thinning =
    if Namectx.is_empty thinning.dom && Namectx.is_empty thinning.im then
      Format.fprintf fmt ""
    else
      Format.fprintf fmt "%a : [%a] ↪ [%a]" pp_map thinning.map Namectx.pp
        thinning.dom Namectx.pp thinning.im

  let to_string = Format.asprintf "%a" pp

  let of_weakening weakening =
    let dom = Weakening.dom weakening in
    let map =
      Util.Pmap.list_to_pmap
      @@ List.map
           (fun local -> (local, Weakening.lookup weakening local))
           (Namectx.get_names dom) in
    { map; dom; im= Weakening.im weakening }

  let id namectx = of_weakening (Weakening.id namectx)
  let dom thinning = thinning.dom
  let im thinning = thinning.im

  (* Display hints are not part of a typing context's semantics. *)
  let equal_context namectx1 namectx2 =
    Namectx.to_pmap namectx1 = Namectx.to_pmap namectx2

  let is_in_dom thinning name = Util.Pmap.mem name thinning.map
  let lookup thinning name = Util.Pmap.lookup_exn name thinning.map

  let lookup_inv thinning context_name =
    match Util.Pmap.select_im context_name thinning.map with
    | [] -> None
    | [ local ] -> Some local
    | _ -> assert false

  let compose thinning1 thinning2 =
    assert (equal_context thinning1.dom thinning2.im);
    let map = Util.Pmap.map_im (lookup thinning1) thinning2.map in
    { map; dom= thinning2.dom; im= thinning1.im }

  let weak_l namectx_l namectx_r =
    of_weakening (Weakening.weak_l namectx_l namectx_r)

  let weak_r namectx_l namectx_r =
    of_weakening (Weakening.weak_r namectx_l namectx_r)

  let has_duplicates names =
    let rec loop = function
      | [] -> false
      | name :: names -> List.mem name names || loop names in
    loop names

  let of_support context support =
    let context_names = Namectx.get_names context in
    if
      has_duplicates support
      || not (List.for_all (fun name -> List.mem name context_names) support)
    then failwith "Renaming.MakeThin.of_support";
    let ordered_support =
      List.filter (fun name -> List.mem name support) context_names in
    let (dom, map_entries) =
      List.fold_left
        (fun (local_ctx, map_entries) context_name ->
          let typ = Namectx.lookup_exn context context_name in
          let (local_name, local_ctx') = Namectx.add_fresh local_ctx "" typ in
          (local_ctx', (local_name, context_name) :: map_entries))
        (Namectx.empty, []) ordered_support in
    let map = Util.Pmap.list_to_pmap (List.rev map_entries) in
    { map; dom; im= context }
end

module MakeWeak (Namectx : Typectx.TYPECTX_LIST) :
  THINNING with module Namectx = Namectx = struct
  module Namectx = Namectx

  type t = { offset: int; dom: Namectx.t; im: Namectx.t }

  let pp fmt renam =
    if Namectx.is_empty renam.dom && Namectx.is_empty renam.im then
      Format.fprintf fmt ""
    else
      Format.fprintf fmt "+%d : [%a] ⇒ [%a]" renam.offset Namectx.pp renam.dom
        Namectx.pp renam.im

  let to_string = Format.asprintf "%a" pp
  let id namectx = { offset= 0; dom= namectx; im= namectx }
  let dom renam = renam.dom
  let im renam = renam.im
  let size namectx = List.length @@ Namectx.get_names namectx
  let equal_context ctx1 ctx2 = Namectx.to_pmap ctx1 = Namectx.to_pmap ctx2

  let compose renam1 renam2 =
    assert (equal_context renam1.dom renam2.im);
    { offset= renam1.offset + renam2.offset; dom= renam2.dom; im= renam1.im }

  let weak_l namectx_l namectx_r =
    { offset= 0; dom= namectx_l; im= Namectx.concat namectx_l namectx_r }

  let weak_r namectx_l namectx_r =
    {
      offset= size namectx_r;
      dom= namectx_l;
      im= Namectx.concat namectx_r namectx_l;
    }

  let is_in_dom renam i = 0 <= i && i < size renam.dom

  let lookup renam i =
    if is_in_dom renam i then i + renam.offset else raise Not_found

  let lookup_inv renam i =
    let local = i - renam.offset in
    if is_in_dom renam local then Some local else None
end

module MakeNoName (Namectx : Typectx.TYPECTX with type Names.name = unit) :
  RENAMING with module Namectx = Namectx = struct
  module Namectx = Namectx

  type t = { dom: Namectx.t; im: Namectx.t }

  let pp fmt renam =
    if Namectx.is_empty renam.dom && Namectx.is_empty renam.im then
      Format.fprintf fmt ""
    else
      Format.fprintf fmt "[%a] ⇒ [%a]" Namectx.pp renam.dom Namectx.pp renam.im

  let to_string = Format.asprintf "%a" pp
  let id namectx = { dom= namectx; im= namectx }
  let dom renam = renam.dom
  let im renam = renam.im

  let compose renam1 renam2 =
    assert (renam1.dom = renam2.im);
    let dom = renam2.dom in
    let im = renam1.im in
    { dom; im }

  let copairing renam1 renam2 =
    assert (renam1.im = renam2.im);
    let dom = Namectx.concat renam1.dom renam2.dom in
    { dom; im= renam1.im }

  let weak_l namectx_l namectx_r =
    { dom= namectx_l; im= Namectx.concat namectx_r namectx_l }
  (* We put things in the other way arround*)

  (* weak_r Δ Γ : Δ → Γ + Δ*)
  let weak_r namectx_l namectx_r =
    { dom= namectx_l; im= Namectx.concat namectx_l namectx_r }
  (* Same here *)

  let sym _namectx_l _namectx_r = failwith "TODO"
  let is_in_dom renam () = not (Namectx.is_empty renam.dom)
  let lookup _renam () = ()

  let add_fresh (renam : t) (_str : string) (typ : Namectx.typ) :
      Namectx.Names.name * t =
    let (nn, lnamectx) = Namectx.singleton typ in
    let renam_nn = weak_r lnamectx renam.im in
    let renam' = { renam with im= Namectx.concat renam.im lnamectx } in
    let nn' = lookup renam_nn nn in
    (nn', copairing renam' renam_nn)
end

module MakeAggregate (* Not used so far *)
    (Namectx1 : Typectx.TYPECTX)
    (Namectx2 : Typectx.TYPECTX)
    (Names :
      Names.NAMES
        with type name = (Namectx1.Names.name, Namectx2.Names.name) Either.t) :
  RENAMING
    with module Namectx.Names = Names
     and type Namectx.t = Namectx1.t * Namectx2.t = struct
  module Namectx = Typectx.Aggregate (Namectx1) (Namectx2) (Names)

  type t = {
    map_l: (Namectx1.Names.name, Namectx1.Names.name) Util.Pmap.pmap;
    map_r: (Namectx2.Names.name, Namectx2.Names.name) Util.Pmap.pmap;
    dom: Namectx.t;
    im: Namectx.t;
  }

  let pp_map pp_name fmt map =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, value) =
      Format.fprintf fmt "%a ↦ %a" pp_name n pp_name value in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt map

  let pp fmt renam =
    Format.fprintf fmt "[%a | %a] : %a ⇒ %a"
      (pp_map Namectx1.Names.pp_name)
      renam.map_l
      (pp_map Namectx2.Names.pp_name)
      renam.map_r Namectx.pp renam.dom Namectx.pp renam.im

  let to_string = Format.asprintf "%a" pp

  let id ((namectx1, namectx2) as namectx) =
    let names1_list = Namectx1.get_names namectx1 in
    let names2_list = Namectx2.get_names namectx2 in
    let map_l =
      Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, nn)) names1_list in
    let map_r =
      Util.Pmap.list_to_pmap @@ List.map (fun nn -> (nn, nn)) names2_list in
    { map_l; map_r; dom= namectx; im= namectx }

  let dom renam = renam.dom
  let im renam = renam.im

  let compose renam1 renam2 =
    assert (renam1.dom = renam2.im);
    let dom = renam2.dom in
    let im = renam1.im in
    let map_l =
      Util.Pmap.map_im
        (fun nn -> Util.Pmap.lookup_exn nn renam1.map_l)
        renam2.map_l in
    let map_r =
      Util.Pmap.map_im
        (fun nn -> Util.Pmap.lookup_exn nn renam1.map_r)
        renam2.map_r in
    { map_l; map_r; dom; im }

  let copairing renam1 renam2 =
    assert (renam1.im = renam2.im);
    let dom = Namectx.concat renam1.dom renam2.dom in
    let map_l = Util.Pmap.concat renam1.map_l renam2.map_l in
    let map_r = Util.Pmap.concat renam1.map_r renam2.map_r in
    { map_l; map_r; dom; im= renam1.im }

  let weak_l namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Namectx.concat namectx_l namectx_r }

  let weak_r namectx_l namectx_r =
    let renam = id namectx_l in
    { renam with im= Namectx.concat namectx_r namectx_l }

  let sym _namectx_l _namectx_r = failwith "TODO"

  let is_in_dom renam = function
    | Either.Left nn' -> Util.Pmap.mem nn' renam.map_l
    | Either.Right nn' -> Util.Pmap.mem nn' renam.map_r

  let lookup renam nn =
    try
      match nn with
      | Either.Left nn' -> Either.Left (Util.Pmap.lookup_exn nn' renam.map_l)
      | Either.Right nn' -> Either.Right (Util.Pmap.lookup_exn nn' renam.map_r)
    with Not_found ->
      Util.Debug.print_debug @@ "The name "
      ^ Namectx.Names.string_of_name nn
      ^ " was not found in the renaming " ^ to_string renam;
      raise Not_found

  let add_fresh (renam : t) (_str : string) (typ : Namectx.typ) :
      Namectx.Names.name * t =
    let (nn, lnamectx) = Namectx.singleton typ in
    let renam_nn = weak_r lnamectx renam.im in
    let renam' = { renam with im= Namectx.concat renam.im lnamectx } in
    let nn' = lookup renam_nn nn in
    (nn', copairing renam' renam_nn)
end

module AggregateWeak
    (Weak1 : WEAKENING)
    (Weak2 : WEAKENING)
    (Namectx :
      Typectx.TYPECTX
        with type Names.name =
          (Weak1.Namectx.Names.name, Weak2.Namectx.Names.name) Either.t
         and type t = Weak1.Namectx.t * Weak2.Namectx.t) :
  WEAKENING with module Namectx = Namectx and type t = Weak1.t * Weak2.t =
struct
  module Namectx = Namectx

  type t = Weak1.t * Weak2.t

  let pp fmt (renam1, renam2) =
    Format.fprintf fmt "[%a | %a]" Weak1.pp renam1 Weak2.pp renam2

  let to_string = Format.asprintf "%a" pp

  let id (namectx1, namectx2) =
    let id1 = Weak1.id namectx1 in
    let id2 = Weak2.id namectx2 in
    (id1, id2)

  let dom (renam1, renam2) = (Weak1.dom renam1, Weak2.dom renam2)
  let im (renam1, renam2) = (Weak1.im renam1, Weak2.im renam2)

  let compose (renam11, renam12) (renam21, renam22) =
    assert (
      Weak1.dom renam11 = Weak1.im renam21
      && Weak2.dom renam12 = Weak2.im renam22);
    let renam1 = Weak1.compose renam11 renam21 in
    let renam2 = Weak2.compose renam12 renam22 in
    (renam1, renam2)

  let weak_l (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    let map1 = Weak1.weak_l namectx1_l namectx1_r in
    let map2 = Weak2.weak_l namectx2_l namectx2_r in
    (map1, map2)

  let weak_r (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    let map1 = Weak1.weak_r namectx1_l namectx1_r in
    let map2 = Weak2.weak_r namectx2_l namectx2_r in
    (map1, map2)

  let is_in_dom (renam1, renam2) = function
    | Either.Left nn' -> Weak1.is_in_dom renam1 nn'
    | Either.Right nn' -> Weak2.is_in_dom renam2 nn'

  let lookup (renam1, renam2) nn =
    match nn with
    | Either.Left nn' -> Either.Left (Weak1.lookup renam1 nn')
    | Either.Right nn' -> Either.Right (Weak2.lookup renam2 nn')
end

module AggregateThin
    (Thin1 : THINNING)
    (Thin2 : THINNING)
    (Namectx :
      Typectx.TYPECTX
        with type Names.name =
          (Thin1.Namectx.Names.name, Thin2.Namectx.Names.name) Either.t
         and type t = Thin1.Namectx.t * Thin2.Namectx.t) :
  THINNING with module Namectx = Namectx and type t = Thin1.t * Thin2.t = struct
  include AggregateWeak (Thin1) (Thin2) (Namectx)

  (* The component operations perform the appropriate context compatibility
     checks; in particular, sparse list thinnings ignore display hints. *)
  let compose (thin11, thin12) (thin21, thin22) =
    (Thin1.compose thin11 thin21, Thin2.compose thin12 thin22)

  let lookup_inv (thin1, thin2) = function
    | Either.Left context_name ->
        Option.map
          (fun local -> Either.Left local)
          (Thin1.lookup_inv thin1 context_name)
    | Either.Right context_name ->
        Option.map
          (fun local -> Either.Right local)
          (Thin2.lookup_inv thin2 context_name)
end

module Aggregate
    (Renam1 : RENAMING)
    (Renam2 : RENAMING)
    (Namectx :
      Typectx.TYPECTX
        with type Names.name =
          (Renam1.Namectx.Names.name, Renam2.Namectx.Names.name) Either.t
         and type t = Renam1.Namectx.t * Renam2.Namectx.t) :
  RENAMING with module Namectx = Namectx and type t = Renam1.t * Renam2.t =
struct
  include AggregateWeak (Renam1) (Renam2) (Namectx)

  let copairing (renam11, renam12) (renam21, renam22) =
    assert (
      Renam1.im renam11 = Renam1.im renam21
      && Renam2.im renam12 = Renam2.im renam22);
    let renam1 = Renam1.copairing renam11 renam21 in
    let renam2 = Renam2.copairing renam12 renam22 in
    (renam1, renam2)

  let sym (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    (Renam1.sym namectx1_l namectx1_r, Renam2.sym namectx2_l namectx2_r)

  (*(* weaken_r (ρ:Δ → Γ) Θ : Δ → Γ + Θ *) *)
  let weaken_r (renam : t) (namectx : Namectx.t) : t =
    let renam' = weak_l (im renam) namectx in
    compose renam' renam

  let add_fresh (renam : t) (_str : string) (typ : Namectx.typ) :
      Namectx.Names.name * t =
    let (nn, lnamectx) = Namectx.singleton typ in
    let renam_nn = weak_r lnamectx (im renam) in
    let renam' = weaken_r renam lnamectx in
    let nn' = lookup renam_nn nn in
    (nn', copairing renam' renam_nn)
end
