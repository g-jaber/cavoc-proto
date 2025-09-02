module type RENAMING = sig
  module Namectx : Typectx.TYPECTX

  type t

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val id : Namectx.t -> t
  val dom : t -> Namectx.t
  val im : t -> Namectx.t
  val compose : t -> t -> t
  val copairing : t -> t -> t

  (* weak_l Δ Γ : Δ → Δ + Γ*)
  val weak_l : Namectx.t -> Namectx.t -> t

  (* weak_r Δ Γ : Δ → Γ + Δ *)
  val weak_r : Namectx.t -> Namectx.t -> t

  (* sym Δ Γ : Δ + Γ → Γ + Δ*)
  val sym : Namectx.t -> Namectx.t -> t
  val lookup : t -> Namectx.Names.name -> Namectx.Names.name
  val add_fresh : t -> string -> Namectx.typ -> Namectx.Names.name * t
  (* The second argument is used to associate a string to the fresh variable *)
end

module type RENAMING_LIST = sig
  include RENAMING with type Namectx.Names.name = int * string
end

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

  let sym _namectx_l _namectx_r = failwith "TODO"
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
      Util.Pmap.list_to_pmap
      @@ List.map (fun ((i, str) as nn) -> (nn, (i + offset, str))) names_l
    in
    { map; dom= namectx_l; im= Namectx.concat namectx_r namectx_l }

  let sym _namectx_l _namectx_r = failwith "TODO"

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
  module Namectx = Namectx

  type t = Renam1.t * Renam2.t

  let pp fmt (renam1, renam2) =
    Format.fprintf fmt "[%a | %a]" Renam1.pp renam1 Renam2.pp renam2

  let to_string = Format.asprintf "%a" pp

  let id (namectx1, namectx2) =
    let id1 = Renam1.id namectx1 in
    let id2 = Renam2.id namectx2 in
    (id1, id2)

  let dom (renam1, renam2) = (Renam1.dom renam1, Renam2.dom renam2)
  let im (renam1, renam2) = (Renam1.im renam1, Renam2.im renam2)

  let compose (renam11, renam12) (renam21, renam22) =
    assert (
      Renam1.dom renam11 = Renam1.im renam21
      && Renam2.dom renam12 = Renam2.im renam22);
    let renam1 = Renam1.compose renam11 renam21 in
    let renam2 = Renam2.compose renam12 renam22 in
    (renam1, renam2)

  let copairing (renam11, renam12) (renam21, renam22) =
    assert (
      Renam1.im renam11 = Renam1.im renam21
      && Renam2.im renam12 = Renam2.im renam22);
    let renam1 = Renam1.copairing renam11 renam21 in
    let renam2 = Renam2.copairing renam12 renam22 in
    (renam1, renam2)

  let weak_l (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    let map1 = Renam1.weak_l namectx1_l namectx1_r in
    let map2 = Renam2.weak_l namectx2_l namectx2_r in
    (map1, map2)

  let weak_r (namectx1_l, namectx2_l) (namectx1_r, namectx2_r) =
    let map1 = Renam1.weak_r namectx1_l namectx1_r in
    let map2 = Renam2.weak_r namectx2_l namectx2_r in
    (map1, map2)

  let sym _namectx_l _namectx_r = failwith "TODO"

  let lookup (renam1, renam2) nn =
    match nn with
    | Either.Left nn' -> Either.Left (Renam1.lookup renam1 nn')
    | Either.Right nn' -> Either.Right (Renam2.lookup renam2 nn')

      (*(* weaken_r (ρ:Δ → Γ) Θ : Δ → Γ + Θ *) *)
  let weaken_r (renam:t) (namectx:Namectx.t) : t =
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
