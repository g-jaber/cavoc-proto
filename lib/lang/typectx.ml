module type TYPECTX = sig
  type name
  type typ
  type t [@@deriving to_yojson]

  val empty : t
  val concat : t -> t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val get_names : t -> name list
  val lookup_exn : t -> name -> typ
  val is_empty : t -> bool
  val is_singleton : t -> name -> typ -> bool
  val is_last : t -> name -> typ -> t option
  val add : t -> name -> typ -> t
  val to_pmap : t -> (name, typ) Util.Pmap.pmap
  val singleton : typ -> name * t
  val mem : t -> name -> bool
  val add_fresh : t -> string -> typ -> name * t
  (* The second argument is used to associate a string to the fresh variable *)

  val map : (typ -> typ) -> t -> t
end

module type TYPECTX_PMAP = sig
  type n
  type ty

  include
    TYPECTX
      with type name = n
       and type typ = ty
       and type t = (n, ty) Util.Pmap.pmap
end

module type TYPECTX_LIST = sig
  type ty

  include TYPECTX with type name = int and type typ = ty and type t = ty list
end

module Make_PMAP
    (Names : Names.NAMES_GEN)
    (Types : sig
      type t [@@deriving to_yojson]

      val pp : Format.formatter -> t -> unit
    end) :
  TYPECTX
    with type name = Names.name
     and type typ = Types.t
     and type t = (Names.name, Types.t) Util.Pmap.pmap = struct
  type name = Names.name
  type typ = Types.t
  type t = (name, Types.t) Util.Pmap.pmap

  let empty = Util.Pmap.empty
  let concat = Util.Pmap.concat

  let pp fmt name_ctx =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_empty fmt () = Format.fprintf fmt "⋅" in
    let pp_pair fmt (n, ty) =
      Format.fprintf fmt "%a : %a" Names.pp_name n Types.pp ty in
    Util.Pmap.pp_pmap ~pp_empty ~pp_sep pp_pair fmt name_ctx

  let to_string = Format.asprintf "%a" pp
  let get_names = Util.Pmap.dom

  let to_yojson name_ctx =
    let to_string (nn, ty) = (Names.string_of_name nn, Types.to_yojson ty) in
    `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string name_ctx)

  let lookup_exn name_ctx nn = Util.Pmap.lookup_exn nn name_ctx
  let add name_ctx nn ty = Util.Pmap.add (nn, ty) name_ctx
  let to_pmap = Fun.id
  let is_empty = Util.Pmap.is_empty
  let is_singleton name_ctx nn ty = Util.Pmap.is_singleton name_ctx (nn, ty)

  let is_last name_ctx nn ty =
    let name_ctx_l = Util.Pmap.to_list name_ctx in
    let rec aux name_ctx_l acc =
      match name_ctx_l with
      | [] -> None
      | [ (nn', ty') ] ->
          if nn = nn' && ty = ty' then
            Some (Util.Pmap.list_to_pmap (List.rev acc))
          else None
      | hd :: name_ctx_l' -> aux name_ctx_l' (hd :: acc) in
    aux name_ctx_l []

  let singleton ty =
    let nn = Names.fresh_name () in
    (nn, Util.Pmap.singleton (nn, ty))

  let mem namectx nn = Util.Pmap.mem nn namectx

  let add_fresh name_ctx str ty =
    let nn = Names.from_string str in
    (nn, Util.Pmap.add (nn, ty) name_ctx)

  let map = Util.Pmap.map_im
end

module Make_List (Types : sig
  type t [@@deriving to_yojson]

  val pp : Format.formatter -> t -> unit
end) :
  TYPECTX
    with type name = int * string
     and type typ = Types.t
     and type t = Types.t list = struct
  type name = int * string
  type typ = Types.t
  type t = Types.t list

  let empty = []
  let concat = List.append

  let pp fmt = function
    | [] -> Format.fprintf fmt "⋅"
    | name_ctx ->
        let pp_sep fmt () = Format.fprintf fmt ", " in
        Format.pp_print_list ~pp_sep Types.pp fmt name_ctx

  let to_string = Format.asprintf "%a" pp
  let get_names = List.mapi (fun i _ -> (i, ""))

  let to_yojson nctx =
    `List (List.mapi (fun i typ -> `Tuple [ `Int i; Types.to_yojson typ ]) nctx)

  let lookup_exn nctx (i, _) = List.nth nctx i
  let is_empty = function [] -> true | _ -> false

  let is_singleton nctx (nn, _) ty =
    match nctx with [ ty' ] when nn = 0 && ty = ty' -> true | _ -> false

  let is_last name_ctx (nn, _) ty =
    let rec aux name_ctx i acc =
      match name_ctx with
      | [] -> None
      | [ ty' ] -> if nn = i && ty = ty' then Some (List.rev acc) else None
      | hd :: name_ctx' -> aux name_ctx' (i + 1) (hd :: acc) in
    aux name_ctx 0 []

  (* The add function always add at the end of the list !!!*)
  let add name_ctx _ ty = name_ctx @ [ ty ]

  let to_pmap name_ctx =
    Util.Pmap.list_to_pmap @@ List.mapi (fun i ty -> ((i, ""), ty)) name_ctx

  let singleton ty = ((0, ""), [ ty ])
  let mem namectx (nn, _) = nn < List.length namectx

  let add_fresh name_ctx str ty =
    let nn = List.length name_ctx in
    ((nn, str), name_ctx @ [ ty ])

  let map = List.map
end

module Aggregate (Namectx1 : TYPECTX) (Namectx2 : TYPECTX) :
  TYPECTX
    with type name = (Namectx1.name, Namectx2.name) Either.t
     and type typ = (Namectx1.typ, Namectx2.typ) Either.t
     and type t = Namectx1.t * Namectx2.t = struct
  type t = Namectx1.t * Namectx2.t [@@deriving to_yojson]
  type name = (Namectx1.name, Namectx2.name) Either.t
  type typ = (Namectx1.typ, Namectx2.typ) Either.t

  let empty = (Namectx1.empty, Namectx2.empty)

  let concat (namectx11, namectx12) (namectx21, namectx22) =
    (Namectx1.concat namectx11 namectx21, Namectx2.concat namectx12 namectx22)

  let pp fmt (namectx1, namectx2) =
    Format.fprintf fmt "(%a,%a)" Namectx1.pp namectx1 Namectx2.pp namectx2

  let to_string = Format.asprintf "%a" pp

  let get_names (namectx1, namectx2) =
    List.map (fun nn -> Either.Left nn) (Namectx1.get_names namectx1)
    @ List.map (fun nn -> Either.Right nn) (Namectx2.get_names namectx2)

  let lookup_exn (namectx1, namectx2) nn =
    match nn with
    | Either.Left nn' -> Either.Left (Namectx1.lookup_exn namectx1 nn')
    | Either.Right nn' -> Either.Right (Namectx2.lookup_exn namectx2 nn')

  let is_empty (namectx1, namectx2) =
    Namectx1.is_empty namectx1 && Namectx2.is_empty namectx2

  let is_singleton (namectx1, namectx2) nn ty =
    match (nn, ty) with
    | (Either.Left nn', Either.Left ty') ->
        Namectx1.is_singleton namectx1 nn' ty'
    | (Either.Right nn', Either.Right ty') ->
        Namectx2.is_singleton namectx2 nn' ty'
    | _ ->
        failwith
          "Error while performing a singleton test on an aggregated context. \
           Please report."

  let is_last (namectx1, namectx2) nn ty =
    let open Util.Monad.Option in
    match (nn, ty) with
    | (Either.Left nn', Either.Left ty') ->
        let* namectx1' = Namectx1.is_last namectx1 nn' ty' in
        return (namectx1', namectx2)
    | (Either.Right nn', Either.Right ty') ->
        let* namectx2' = Namectx2.is_last namectx2 nn' ty' in
        return (namectx1, namectx2')
    | _ ->
        failwith
          "Error while performing a last test on an aggregated context. Please \
           report."

  let add (namectx1, namectx2) nn ty =
    match (nn, ty) with
    | (Either.Left nn', Either.Left ty') ->
        let namectx1' = Namectx1.add namectx1 nn' ty' in
        (namectx1', namectx2)
    | (Either.Right nn', Either.Right ty') ->
        let namectx2' = Namectx2.add namectx2 nn' ty' in
        (namectx1, namectx2')
    | _ ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of names. \
           Please report."

  let mem (namectx1, namectx2) nn =
    match nn with
    | Either.Left nn' -> Namectx1.mem namectx1 nn'
    | Either.Right nn' -> Namectx2.mem namectx2 nn'

  let to_pmap (namectx1, namectx2) =
    let namectx1_pmap = Namectx1.to_pmap namectx1 in
    let namectx1_pmap' =
      Util.Pmap.map
        (fun (nn, ty) -> (Either.Left nn, Either.Left ty))
        namectx1_pmap in
    let namectx2_pmap = Namectx2.to_pmap namectx2 in
    let namectx2_pmap' =
      Util.Pmap.map
        (fun (nn, ty) -> (Either.Right nn, Either.Right ty))
        namectx2_pmap in
    Util.Pmap.concat namectx1_pmap' namectx2_pmap'

  let singleton ty =
    match ty with
    | Either.Left ty' ->
        let (nn, namectx) = Namectx1.singleton ty' in
        (Either.Left nn, (namectx, Namectx2.empty))
    | Either.Right ty' ->
        let (nn, namectx2) = Namectx2.singleton ty' in
        (Either.Right nn, (Namectx1.empty, namectx2))

  let add_fresh (namectx1, namectx2) str ty =
    match ty with
    | Either.Left ty' ->
        let (nn, namectx) = Namectx1.add_fresh namectx1 str ty' in
        (Either.Left nn, (namectx, namectx2))
    | Either.Right ty' ->
        let (nn, namectx) = Namectx2.add_fresh namectx2 str ty' in
        (Either.Right nn, (namectx1, namectx))

  let map f (namectx1, namectx2) =
    let f1 ty =
      match f (Either.Left ty) with
      | Either.Left ty' -> ty'
      | Either.Right _ty' ->
          failwith
            "Using a map with a function that does not preserve components. \
             Please report." in
    let f2 ty =
      match f (Either.Right ty) with
      | Either.Right ty' -> ty'
      | Either.Left _ty' ->
          failwith
            "Using a map with a function that does not preserve components. \
             Please report." in
    (Namectx1.map f1 namectx1, Namectx2.map f2 namectx2)
end

module AggregateCommon
    (Namectx1 : TYPECTX)
    (Namectx2 : TYPECTX with type typ = Namectx1.typ)
    (EmbedNames : sig
      type t

      val embed1 : Namectx1.name -> t
      val embed2 : Namectx2.name -> t
      val extract1 : t -> Namectx1.name option
      val extract2 : t -> Namectx2.name option
    end)
    (ClassifyTyp : sig
      val classify : Namectx1.typ -> bool
    end) :
  TYPECTX
    with type name = EmbedNames.t
     and type typ = Namectx1.typ
     and type t = Namectx1.t * Namectx2.t = struct
  type t = Namectx1.t * Namectx2.t [@@deriving to_yojson]
  type name = EmbedNames.t
  type typ = Namectx1.typ

  let empty = (Namectx1.empty, Namectx2.empty)

  let concat (namectx11, namectx12) (namectx21, namectx22) =
    (Namectx1.concat namectx11 namectx21, Namectx2.concat namectx12 namectx22)

  let pp fmt (namectx1, namectx2) =
    Format.fprintf fmt "(%a,%a)" Namectx1.pp namectx1 Namectx2.pp namectx2

  let to_string = Format.asprintf "%a" pp

  let get_names (namectx1, namectx2) =
    List.map (fun nn -> EmbedNames.embed1 nn) (Namectx1.get_names namectx1)
    @ List.map (fun nn -> EmbedNames.embed2 nn) (Namectx2.get_names namectx2)

  let lookup_exn (namectx1, namectx2) nn =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (Some nn', None) -> Namectx1.lookup_exn namectx1 nn'
    | (None, Some nn') -> Namectx2.lookup_exn namectx2 nn'
    | _ ->
        failwith
          "Error while performing a lookup on an aggregated context. Please \
           report."

  let is_empty (namectx1, namectx2) =
    Namectx1.is_empty namectx1 && Namectx2.is_empty namectx2

  let is_singleton (namectx1, namectx2) nn ty =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (Some nn', None) -> Namectx1.is_singleton namectx1 nn' ty
    | (None, Some nn') -> Namectx2.is_singleton namectx2 nn' ty
    | _ ->
        failwith
          "Error while performing a singleton test on an aggregated context. \
           Please report."

  let is_last (namectx1, namectx2) nn ty =
    let open Util.Monad.Option in
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (Some nn', None) ->
        let* namectx1' = Namectx1.is_last namectx1 nn' ty in
        return (namectx1', namectx2)
    | (None, Some nn') ->
        let* namectx2' = Namectx2.is_last namectx2 nn' ty in
        return (namectx1, namectx2')
    | _ ->
        failwith
          "Error while performing a singleton test on an aggregated context. \
           Please report."

  let add (namectx1, namectx2) nn ty =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (None, Some nn') ->
        let namectx2' = Namectx2.add namectx2 nn' ty in
        (namectx1, namectx2')
    | (Some nn', None) ->
        let namectx1' = Namectx1.add namectx1 nn' ty in
        (namectx1', namectx2)
    | _ ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of names. \
           Please report."

  let mem (namectx1, namectx2) nn =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (None, Some nn') -> Namectx2.mem namectx2 nn'
    | (Some nn', None) -> Namectx1.mem namectx1 nn'
    | _ ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of names. \
           Please report."

  let to_pmap (namectx1, namectx2) =
    let namectx1_pmap = Namectx1.to_pmap namectx1 in
    let namectx1_pmap' =
      Util.Pmap.map (fun (nn, ty) -> (EmbedNames.embed1 nn, ty)) namectx1_pmap
    in
    let namectx2_pmap = Namectx2.to_pmap namectx2 in
    let namectx2_pmap' =
      Util.Pmap.map (fun (cn, ty) -> (EmbedNames.embed2 cn, ty)) namectx2_pmap
    in
    Util.Pmap.concat namectx1_pmap' namectx2_pmap'

  let singleton ty =
    if ClassifyTyp.classify ty then
      let (nn, namectx) = Namectx1.singleton ty in
      (EmbedNames.embed1 nn, (namectx, Namectx2.empty))
    else
      let (nn, namectx2) = Namectx2.singleton ty in
      (EmbedNames.embed2 nn, (Namectx1.empty, namectx2))

  let add_fresh (namectx1, namectx2) str ty =
    if ClassifyTyp.classify ty then
      let (nn, namectx) = Namectx1.add_fresh namectx1 str ty in
      (EmbedNames.embed1 nn, (namectx, namectx2))
    else
      let (nn, namectx) = Namectx2.add_fresh namectx2 str ty in
      (EmbedNames.embed2 nn, (namectx1, namectx))

  let map f (namectx1, namectx2) =
    (Namectx1.map f namectx1, Namectx2.map f namectx2)
end

module AggregateCommonType (* Not Used *)
    (Namectx1 : TYPECTX)
    (Namectx2 : TYPECTX with type typ = Namectx1.typ)
    (ClassifyTyp : sig
      val classify : Namectx1.typ -> bool
    end) : TYPECTX = struct
  type name = N1 of Namectx1.name | N2 of Namectx2.name
  type typ = Namectx1.typ
  type t = Namectx1.t * Namectx2.t [@@deriving to_yojson]

  let empty = (Namectx1.empty, Namectx2.empty)

  let concat (namectx11, namectx12) (namectx21, namectx22) =
    (Namectx1.concat namectx11 namectx21, Namectx2.concat namectx12 namectx22)

  let pp fmt (namectx1, namectx2) =
    Format.fprintf fmt "(%a,%a)" Namectx1.pp namectx1 Namectx2.pp namectx2

  let to_string = Format.asprintf "%a" pp

  let get_names (namectx1, namectx2) =
    List.map (fun nn -> N1 nn) (Namectx1.get_names namectx1)
    @ List.map (fun nn -> N2 nn) (Namectx2.get_names namectx2)

  let lookup_exn (namectx1, namectx2) = function
    | N1 nn -> Namectx1.lookup_exn namectx1 nn
    | N2 nn -> Namectx2.lookup_exn namectx2 nn

  let is_empty (namectx1, namectx2) =
    Namectx1.is_empty namectx1 && Namectx2.is_empty namectx2

  let is_singleton (namectx1, namectx2) nn ty =
    match nn with
    | N1 nn' -> Namectx1.is_singleton namectx1 nn' ty
    | N2 nn' -> Namectx2.is_singleton namectx2 nn' ty

  let is_last (namectx1, namectx2) nn ty =
    let open Util.Monad.Option in
    match nn with
    | N1 nn' ->
        let* namectx1' = Namectx1.is_last namectx1 nn' ty in
        return (namectx1', namectx2)
    | N2 nn' ->
        let* namectx2' = Namectx2.is_last namectx2 nn' ty in
        return (namectx1, namectx2')

  let add (namectx1, namectx2) nn ty =
    match nn with
    | N2 cn ->
        let namectx2' = Namectx2.add namectx2 cn ty in
        (namectx1, namectx2')
    | N1 nn ->
        let namectx1' = Namectx1.add namectx1 nn ty in
        (namectx1', namectx2)

  let mem (namectx1, namectx2) = function
    | N1 nn -> Namectx1.mem namectx1 nn
    | N2 cn -> Namectx2.mem namectx2 cn

  let to_pmap (namectx1, namectx2) =
    let namectx1_pmap = Namectx1.to_pmap namectx1 in
    let namectx1_pmap' =
      Util.Pmap.map (fun (nn, ty) -> (N1 nn, ty)) namectx1_pmap in
    let namectx2_pmap = Namectx2.to_pmap namectx2 in
    let namectx2_pmap' =
      Util.Pmap.map (fun (cn, ty) -> (N2 cn, ty)) namectx2_pmap in
    Util.Pmap.concat namectx1_pmap' namectx2_pmap'

  let singleton ty =
    if ClassifyTyp.classify ty then
      let (nn, namectx) = Namectx1.singleton ty in
      (N1 nn, (namectx, Namectx2.empty))
    else
      let (nn, namectx2) = Namectx2.singleton ty in
      (N2 nn, (Namectx1.empty, namectx2))

  let add_fresh (namectx1, namectx2) str ty =
    if ClassifyTyp.classify ty then
      let (nn, namectx) = Namectx1.add_fresh namectx1 str ty in
      (N1 nn, (namectx, namectx2))
    else
      let (nn, namectx) = Namectx2.add_fresh namectx2 str ty in
      (N2 nn, (namectx1, namectx))

  let map f (namectx1, namectx2) =
    (Namectx1.map f namectx1, Namectx2.map f namectx2)
end

module AggregateDisjoint (* Not used *)
    (Namectx1 : TYPECTX)
    (Namectx2 : TYPECTX)
    (EmbedNames : sig
      type t

      val embed1 : Namectx1.name -> t
      val embed2 : Namectx2.name -> t
      val extract1 : t -> Namectx1.name option
      val extract2 : t -> Namectx2.name option
    end)
    (EmbedTypes : sig
      type t [@@deriving to_yojson]

      val embed1 : Namectx1.typ -> t
      val embed2 : Namectx2.typ -> t
      val extract1 : t -> Namectx1.typ option
      val extract2 : t -> Namectx2.typ option
    end) :
  TYPECTX
    with type name = EmbedNames.t
     and type typ = EmbedTypes.t
     and type t = Namectx1.t * Namectx2.t = struct
  type t = Namectx1.t * Namectx2.t [@@deriving to_yojson]
  type name = EmbedNames.t
  type typ = EmbedTypes.t

  let empty = (Namectx1.empty, Namectx2.empty)

  let concat (namectx11, namectx12) (namectx21, namectx22) =
    (Namectx1.concat namectx11 namectx21, Namectx2.concat namectx12 namectx22)

  let pp fmt (namectx1, namectx2) =
    Format.fprintf fmt "(%a,%a)" Namectx1.pp namectx1 Namectx2.pp namectx2

  let to_string = Format.asprintf "%a" pp

  let get_names (namectx1, namectx2) =
    List.map (fun nn -> EmbedNames.embed1 nn) (Namectx1.get_names namectx1)
    @ List.map (fun nn -> EmbedNames.embed2 nn) (Namectx2.get_names namectx2)

  let lookup_exn (namectx1, namectx2) nn =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (Some nn', None) -> EmbedTypes.embed1 @@ Namectx1.lookup_exn namectx1 nn'
    | (None, Some nn') -> EmbedTypes.embed2 @@ Namectx2.lookup_exn namectx2 nn'
    | _ ->
        failwith
          "Error while performing a lookup on an aggregated context. Please \
           report."

  let is_empty (namectx1, namectx2) =
    Namectx1.is_empty namectx1 && Namectx2.is_empty namectx2

  let is_singleton (namectx1, namectx2) nn ty =
    match
      ( EmbedNames.extract1 nn,
        EmbedTypes.extract1 ty,
        EmbedNames.extract2 nn,
        EmbedTypes.extract2 ty )
    with
    | (Some nn', Some ty', None, None) -> Namectx1.is_singleton namectx1 nn' ty'
    | (None, None, Some nn', Some ty') -> Namectx2.is_singleton namectx2 nn' ty'
    | _ ->
        failwith
          "Error while performing a singleton test on an aggregated context. \
           Please report."

  let is_last (namectx1, namectx2) nn ty =
    let open Util.Monad.Option in
    match
      ( EmbedNames.extract1 nn,
        EmbedTypes.extract1 ty,
        EmbedNames.extract2 nn,
        EmbedTypes.extract2 ty )
    with
    | (Some nn', Some ty', None, None) ->
        let* namectx1' = Namectx1.is_last namectx1 nn' ty' in
        return (namectx1', namectx2)
    | (None, None, Some nn', Some ty') ->
        let* namectx2' = Namectx2.is_last namectx2 nn' ty' in
        return (namectx1, namectx2')
    | _ ->
        failwith
          "Error while performing a last test on an aggregated context. Please \
           report."

  let add (namectx1, namectx2) nn ty =
    match
      ( EmbedNames.extract1 nn,
        EmbedTypes.extract1 ty,
        EmbedNames.extract2 nn,
        EmbedTypes.extract2 ty )
    with
    | (None, None, Some nn', Some ty') ->
        let namectx2' = Namectx2.add namectx2 nn' ty' in
        (namectx1, namectx2')
    | (Some nn', Some ty', None, None) ->
        let namectx1' = Namectx1.add namectx1 nn' ty' in
        (namectx1', namectx2)
    | _ ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of names. \
           Please report."

  let mem (namectx1, namectx2) nn =
    match (EmbedNames.extract1 nn, EmbedNames.extract2 nn) with
    | (None, Some nn') -> Namectx2.mem namectx2 nn'
    | (Some nn', None) -> Namectx1.mem namectx1 nn'
    | _ ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of names. \
           Please report."

  let to_pmap (namectx1, namectx2) =
    let namectx1_pmap = Namectx1.to_pmap namectx1 in
    let namectx1_pmap' =
      Util.Pmap.map
        (fun (nn, ty) -> (EmbedNames.embed1 nn, EmbedTypes.embed1 ty))
        namectx1_pmap in
    let namectx2_pmap = Namectx2.to_pmap namectx2 in
    let namectx2_pmap' =
      Util.Pmap.map
        (fun (cn, ty) -> (EmbedNames.embed2 cn, EmbedTypes.embed2 ty))
        namectx2_pmap in
    Util.Pmap.concat namectx1_pmap' namectx2_pmap'

  let singleton ty =
    match (EmbedTypes.extract1 ty, EmbedTypes.extract2 ty) with
    | (None, Some ty) ->
        let (nn, namectx2) = Namectx2.singleton ty in
        (EmbedNames.embed2 nn, (Namectx1.empty, namectx2))
    | (Some ty, None) ->
        let (nn, namectx1) = Namectx1.singleton ty in
        (EmbedNames.embed1 nn, (namectx1, Namectx2.empty))
    | (Some _, Some _) ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of types. \
           Please report."
    | (None, None) ->
        failwith
          "Aggregating two typing contexts with missing types. Please report."

  let add_fresh (namectx1, namectx2) str ty =
    match (EmbedTypes.extract1 ty, EmbedTypes.extract2 ty) with
    | (None, Some ty) ->
        let (nn, namectx2') = Namectx2.add_fresh namectx2 str ty in
        (EmbedNames.embed2 nn, (namectx1, namectx2'))
    | (Some ty, None) ->
        let (nn, namectx') = Namectx1.add_fresh namectx1 str ty in
        (EmbedNames.embed1 nn, (namectx', namectx2))
    | (Some _, Some _) ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of types. \
           Please report."
    | (None, None) ->
        failwith
          "Aggregating two typing contexts with missing types. Please report."

  let map f (namectx1, namectx2) =
    let f_lift1 ty =
      match EmbedTypes.extract1 @@ f (EmbedTypes.embed1 ty) with
      | Some ty' -> ty'
      | None -> failwith "Map does not preserve type embeding. Please report"
    in
    let f_lift2 ty =
      match EmbedTypes.extract2 @@ f (EmbedTypes.embed2 ty) with
      | Some ty' -> ty'
      | None -> failwith "Map does not preserve type embeding. Please report"
    in
    (Namectx1.map f_lift1 namectx1, Namectx2.map f_lift2 namectx2)
end

module AggregateDisjointTypes (* Not used *)
    (Namectx1 : TYPECTX)
    (Namectx2 : TYPECTX)
    (Types : sig
      type t [@@deriving to_yojson]

      val embed1 : Namectx1.typ -> t
      val embed2 : Namectx2.typ -> t
      val extract1 : t -> Namectx1.typ option
      val extract2 : t -> Namectx2.typ option
    end) =
struct
  type name = N1 of Namectx1.name | N2 of Namectx2.name
  type typ = Types.t

  let empty = (Namectx1.empty, Namectx2.empty)

  let concat (namectx1, cnamectx1) (namectx2, cnamectx2) =
    (Namectx1.concat namectx1 namectx2, Namectx2.concat cnamectx1 cnamectx2)

  let pp fmt (namectx, cnamectx) =
    Format.fprintf fmt "(%a,%a)" Namectx1.pp namectx Namectx2.pp cnamectx

  let to_string = Format.asprintf "%a" pp

  let get_names (namectx, cnamectx) =
    List.map (fun nn -> N1 nn) (Namectx1.get_names namectx)
    @ List.map (fun nn -> N2 nn) (Namectx2.get_names cnamectx)

  let lookup_exn (namectx, cnamectx) = function
    | N2 cn -> Types.embed2 (Namectx2.lookup_exn cnamectx cn)
    | N1 nn -> Types.embed1 (Namectx1.lookup_exn namectx nn)

  let is_empty (namectx1, namectx2) =
    Namectx1.is_empty namectx1 && Namectx2.is_empty namectx2

  let is_singleton (namectx1, namectx2) nn ty =
    match (nn, Types.extract1 ty, Types.extract2 ty) with
    | (N1 nn', Some ty', _) -> Namectx1.is_singleton namectx1 nn' ty'
    | (N2 nn', None, Some ty') -> Namectx2.is_singleton namectx2 nn' ty'
    | _ ->
        failwith
          "Error testing singleton on aggregated contexts. Please report."

  let add (namectx, cnamectx) nn nty =
    match (nn, Types.extract1 nty, Types.extract2 nty) with
    | (N2 cn, None, Some ty) ->
        let cnamectx' = Namectx2.add cnamectx cn ty in
        (namectx, cnamectx')
    | (N1 nn, Some ty, None) ->
        let namectx' = Namectx1.add namectx nn ty in
        (namectx', cnamectx)
    | _ ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of types. \
           Please report."

  let mem (namectx, cnamectx) = function
    | N1 nn -> Namectx1.mem namectx nn
    | N2 cn -> Namectx2.mem cnamectx cn

  let to_pmap (namectx, cnamectx) =
    let namectx_pmap = Namectx1.to_pmap namectx in
    let namectx_pmap' =
      Util.Pmap.map (fun (nn, ty) -> (N1 nn, Types.embed1 ty)) namectx_pmap
    in
    let cnamectx_pmap = Namectx2.to_pmap cnamectx in
    let cnamectx_pmap' =
      Util.Pmap.map (fun (cn, ty) -> (N2 cn, Types.embed2 ty)) cnamectx_pmap
    in
    Util.Pmap.concat namectx_pmap' cnamectx_pmap'

  let singleton ty =
    match (Types.extract1 ty, Types.extract2 ty) with
    | (None, Some ty) ->
        let (cn, cnamectx) = Namectx2.singleton ty in
        (N2 cn, (Namectx1.empty, cnamectx))
    | (Some ty, None) ->
        let (nn, namectx) = Namectx1.singleton ty in
        (N1 nn, (namectx, Namectx2.empty))
    | (Some _, Some _) ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of types. \
           Please report."
    | (None, None) ->
        failwith
          "Aggregating two typing contexts with missing types. Please report."

  let add_fresh (namectx, cnamectx) str ty =
    match (Types.extract1 ty, Types.extract2 ty) with
    | (None, Some ty) ->
        let (cn, cnamectx') = Namectx2.add_fresh cnamectx str ty in
        (N2 cn, (namectx, cnamectx'))
    | (Some ty, None) ->
        let (nn, namectx') = Namectx1.add_fresh namectx str ty in
        (N1 nn, (namectx', cnamectx))
    | (Some _, Some _) ->
        failwith
          "Aggregating two typing contexts with non-disjoint set of types. \
           Please report."
    | (None, None) ->
        failwith
          "Aggregating two typing contexts with missing types. Please report."
end
