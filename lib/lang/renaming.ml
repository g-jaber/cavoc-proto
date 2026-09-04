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

  (* The partial inverse of [lookup]: None exactly outside the image. *)
  val lookup_inv : t -> Namectx.Names.name -> Namectx.Names.name option

  (* of_support Γ names : Δ ↪ Γ, with Δ the given names in the order of Γ. *)
  val of_support : Namectx.t -> Namectx.Names.name list -> t
end

(* Renamings are general maps between contexts, which may identify names. *)
module type RENAMING = sig
  include WEAKENING

  val copairing : t -> t -> t

  (* sym Δ Γ : Δ + Γ → Γ + Δ*)
  val sym : Namectx.t -> Namectx.t -> t

  (* The string names the fresh variable for display. *)
  val add_fresh : t -> string -> Namectx.typ -> Namectx.Names.name * t

  (* tabulate Δ Γ f : Δ → Γ maps each name of Δ along f, into Γ. *)
  val tabulate :
    Namectx.t -> Namectx.t -> (Namectx.Names.name -> Namectx.Names.name) -> t
end

(* Injective renamings: thinnings with the extra constructions provided by the renaming signature. *)
module type INJECTIVE_RENAMING = sig
  include THINNING

  (* the two arguments of copairing must have disjoint images. *)
  val copairing : t -> t -> t
  val sym : Namectx.t -> Namectx.t -> t

  (* concat ρ1 ρ2 : Δ1 + Δ2 → Γ1 + Γ2. *)
  val concat : t -> t -> t
  val add_fresh : t -> string -> Namectx.typ -> Namectx.Names.name * t

  (* the function argument should be an injective map *)
  val tabulate :
    Namectx.t -> Namectx.t -> (Namectx.Names.name -> Namectx.Names.name) -> t
end

(* Renamings over names drawn from a global gensym. *)
module MakeGensymRenaming (Namectx : Typectx.TYPECTX) :
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

  let tabulate dom im f =
    let map =
      Util.Pmap.list_to_pmap
      @@ List.map (fun nn -> (nn, f nn)) (Namectx.get_names dom) in
    { map; dom; im }
end

(* Weakenings over de Bruijn levels, as an offset. *)
module MakeDeBruijnWeakening (Namectx : Typectx.TYPECTX_LIST) :
  WEAKENING with module Namectx = Namectx = struct
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
  let size namectx = List.length (Namectx.get_names namectx)

  let compose renam1 renam2 =
    assert (Namectx.to_pmap renam1.dom = Namectx.to_pmap renam2.im);
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
end

(* Weakenings over unit names: the contexts alone. *)
module MakeUnitWeakening
    (Namectx : Typectx.TYPECTX with type Names.name = unit) :
  WEAKENING with module Namectx = Namectx = struct
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
    { dom= renam2.dom; im= renam1.im }

  let weak_l namectx_l namectx_r =
    { dom= namectx_l; im= Namectx.concat namectx_l namectx_r }

  let weak_r namectx_l namectx_r =
    { dom= namectx_l; im= Namectx.concat namectx_r namectx_l }

  let is_in_dom renam () = not (Namectx.is_empty renam.dom)
  let lookup _renam () = ()
end

(* Injective renamings over any weakening, as a sparse map; the weakening
   provides the level arithmetic. *)
module MakeInjectiveRenaming (Weakening : WEAKENING) :
  INJECTIVE_RENAMING with module Namectx = Weakening.Namectx = struct
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

  let pp fmt renam =
    if Namectx.is_empty renam.dom && Namectx.is_empty renam.im then
      Format.fprintf fmt ""
    else
      Format.fprintf fmt "%a : [%a] ↪ [%a]" pp_map renam.map Namectx.pp
        renam.dom Namectx.pp renam.im

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
  let dom renam = renam.dom
  let im renam = renam.im

  (* Display hints are not part of a typing context's semantics. *)
  let equal_context namectx1 namectx2 =
    Namectx.to_pmap namectx1 = Namectx.to_pmap namectx2

  let is_in_dom renam name = Util.Pmap.mem name renam.map
  let lookup renam name = Util.Pmap.lookup_exn name renam.map

  let lookup_inv renam context_name =
    match Util.Pmap.select_im context_name renam.map with
    | [] -> None
    | [ local ] -> Some local
    | _ -> failwith "Renaming.lookup_inv: the renaming is not injective"

  let compose renam1 renam2 =
    assert (equal_context renam1.dom renam2.im);
    let map = Util.Pmap.map_im (lookup renam1) renam2.map in
    { map; dom= renam2.dom; im= renam1.im }

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
    then failwith "Renaming.of_support";
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

  (* The names of the second domain are read through its weakening into the
     concatenation of the two domains. *)
  let copairing renam1 renam2 =
    assert (equal_context renam1.im renam2.im);
    let dom = Namectx.concat renam1.dom renam2.dom in
    let shift = Weakening.weak_r renam2.dom renam1.dom in
    let map2 =
      Util.Pmap.list_to_pmap
      @@ List.map
           (fun (local, context_name) ->
             (Weakening.lookup shift local, context_name))
           (Util.Pmap.to_list renam2.map) in
    { map= Util.Pmap.concat renam1.map map2; dom; im= renam1.im }

  let sym namectx_l namectx_r =
    copairing (weak_r namectx_l namectx_r) (weak_l namectx_r namectx_l)

  let concat renam1 renam2 =
    copairing
      (compose (weak_l renam1.im renam2.im) renam1)
      (compose (weak_r renam2.im renam1.im) renam2)

  let add_fresh (renam : t) (_str : string) (typ : Namectx.typ) :
      Namectx.Names.name * t =
    let (local, lnamectx) = Namectx.singleton typ in
    let renam_fresh = weak_r lnamectx renam.im in
    let renam' = { renam with im= Namectx.concat renam.im lnamectx } in
    (lookup renam_fresh local, copairing renam' renam_fresh)

  let tabulate dom im f =
    let map =
      Util.Pmap.list_to_pmap
      @@ List.map (fun name -> (name, f name)) (Namectx.get_names dom) in
    { map; dom; im }
end

(* Injective renamings over an aggregate context, componentwise. *)
module AggregateInjectiveRenaming
    (Renam1 : INJECTIVE_RENAMING)
    (Renam2 : INJECTIVE_RENAMING)
    (Namectx :
      Typectx.TYPECTX
        with type Names.name =
          (Renam1.Namectx.Names.name, Renam2.Namectx.Names.name) Either.t
         and type t = Renam1.Namectx.t * Renam2.Namectx.t) :
  INJECTIVE_RENAMING
    with module Namectx = Namectx
     and type t = Renam1.t * Renam2.t = struct
  module Namectx = Namectx

  type t = Renam1.t * Renam2.t

  let pp fmt (renam1, renam2) =
    Format.fprintf fmt "[%a | %a]" Renam1.pp renam1 Renam2.pp renam2

  let to_string = Format.asprintf "%a" pp
  let id (namectx1, namectx2) = (Renam1.id namectx1, Renam2.id namectx2)
  let dom (renam1, renam2) = (Renam1.dom renam1, Renam2.dom renam2)
  let im (renam1, renam2) = (Renam1.im renam1, Renam2.im renam2)

  let compose (renam11, renam12) (renam21, renam22) =
    (Renam1.compose renam11 renam21, Renam2.compose renam12 renam22)

  let weak_l (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    (Renam1.weak_l namectx1_l namectx1_r, Renam2.weak_l namectx2_l namectx2_r)

  let weak_r (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    (Renam1.weak_r namectx1_l namectx1_r, Renam2.weak_r namectx2_l namectx2_r)

  let is_in_dom (renam1, renam2) = function
    | Either.Left name -> Renam1.is_in_dom renam1 name
    | Either.Right name -> Renam2.is_in_dom renam2 name

  let lookup (renam1, renam2) = function
    | Either.Left name -> Either.Left (Renam1.lookup renam1 name)
    | Either.Right name -> Either.Right (Renam2.lookup renam2 name)

  let lookup_inv (renam1, renam2) = function
    | Either.Left context_name ->
        Option.map
          (fun local -> Either.Left local)
          (Renam1.lookup_inv renam1 context_name)
    | Either.Right context_name ->
        Option.map
          (fun local -> Either.Right local)
          (Renam2.lookup_inv renam2 context_name)

  let of_support (context1, context2) support =
    ( Renam1.of_support context1 (List.filter_map Either.find_left support),
      Renam2.of_support context2 (List.filter_map Either.find_right support) )

  let copairing (renam11, renam12) (renam21, renam22) =
    (Renam1.copairing renam11 renam21, Renam2.copairing renam12 renam22)

  let sym (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    (Renam1.sym namectx1_l namectx1_r, Renam2.sym namectx2_l namectx2_r)

  let concat (renam11, renam12) (renam21, renam22) =
    (Renam1.concat renam11 renam21, Renam2.concat renam12 renam22)

  let weaken_r (renam : t) (namectx : Namectx.t) : t =
    compose (weak_l (im renam) namectx) renam

  let add_fresh (renam : t) (_str : string) (typ : Namectx.typ) :
      Namectx.Names.name * t =
    let (name, lnamectx) = Namectx.singleton typ in
    let renam_fresh = weak_r lnamectx (im renam) in
    let renam' = weaken_r renam lnamectx in
    (lookup renam_fresh name, copairing renam' renam_fresh)

  let tabulate (dom1, dom2) (im1, im2) f =
    ( Renam1.tabulate dom1 im1 (fun name ->
          match f (Either.Left name) with
          | Either.Left name' -> name'
          | Either.Right _ -> failwith "Renaming.tabulate: a name changes sort"),
      Renam2.tabulate dom2 im2 (fun name ->
          match f (Either.Right name) with
          | Either.Right name' -> name'
          | Either.Left _ -> failwith "Renaming.tabulate: a name changes sort")
    )
end
