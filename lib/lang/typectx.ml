module type TYPECTX = sig
  module Names : Names.NAMES

  type typ
  type t [@@deriving to_yojson]

  val empty : t
  val concat : t -> t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val get_names : t -> Names.name list
  val lookup_exn : t -> Names.name -> typ
  val is_empty : t -> bool
  val is_singleton : t -> Names.name -> typ -> bool
  val is_last : t -> Names.name -> typ -> t option
  val to_pmap : t -> (Names.name, typ) Util.Pmap.pmap
  val singleton : typ -> Names.name * t
  val add_fresh : t -> string -> typ -> Names.name * t
  (* The second argument is used to associate a string to the fresh variable *)

  val map : (typ -> typ) -> t -> t
end

module type TYPECTX_PMAP = sig
  type n
  type typ

  include
    TYPECTX
      with type Names.name = n
       and type typ := typ
       and type t = (n, typ) Util.Pmap.pmap
end

module type TYPECTX_LIST = sig
  include TYPECTX with type Names.name = int * string
  (*
  type typ

  include
    TYPECTX
      with type Names.name = int * string
       and type typ := typ
       and type t = typ list*)
end

module Make_PMAP
    (Names : Names.NAMES_GEN)
    (Types : sig
      type t [@@deriving to_yojson]

      val pp : Format.formatter -> t -> unit
    end) : TYPECTX with module Names = Names and type typ = Types.t = struct
  module Names = Names

  type typ = Types.t
  type t = (Names.name, Types.t) Util.Pmap.pmap

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

  let add_fresh name_ctx str ty =
    let nn = Names.from_string str in
    (nn, Util.Pmap.add (nn, ty) name_ctx)

  let map = Util.Pmap.map_im
end

module Make_List
    (Names : Names.NAMES_INT)
    (Types : sig
      type t [@@deriving to_yojson]

      val pp : Format.formatter -> t -> unit
    end) :
  TYPECTX
    with module Names = Names
     and type typ = Types.t = struct
  module Names = Names

  type typ = Types.t
  type t = (string*Types.t) list

  let empty = []
  let concat = List.append

  let pp fmt = function
    | [] -> Format.fprintf fmt "⋅"
    | name_ctx ->
        let pp_sep fmt () = Format.fprintf fmt ", " in
        Format.pp_print_list ~pp_sep (fun fmt (_str,typ) -> Types.pp fmt typ) fmt name_ctx

  let to_string = Format.asprintf "%a" pp
  let get_names = List.mapi (fun i (str,_typ) -> (i, str))

  let to_yojson nctx =
    `List (List.mapi (fun i (_str,typ) -> `List [ `Int i; Types.to_yojson typ ]) nctx)

  let lookup_exn nctx (i, _) = snd @@ List.nth nctx i
  let is_empty = function [] -> true | _ -> false

  let is_singleton nctx (nn, _) ty =
    match nctx with [ (_,ty') ] when nn = 0 && ty = ty' -> true | _ -> false

  let is_last name_ctx (nn, _) ty =
    let rec aux name_ctx i acc =
      match name_ctx with
      | [] -> None
      | [ (_,ty') ] -> if nn = i && ty = ty' then Some (List.rev acc) else None
      | hd :: name_ctx' -> aux name_ctx' (i + 1) (hd :: acc) in
    aux name_ctx 0 []

  let to_pmap name_ctx =
    Util.Pmap.list_to_pmap @@ List.mapi (fun i (str,ty) -> ((i, str), ty)) name_ctx

  let singleton ty = ((0, ""), [ ("",ty) ])

  let add_fresh name_ctx str ty =
    let nn = List.length name_ctx in
    ((nn, str), name_ctx @ [ (str,ty) ])

  let map f = List.map (fun (str,ty) -> (str,f ty))
end

module Aggregate
    (Namectx1 : TYPECTX)
    (Namectx2 : TYPECTX)
    (Names :
      Names.NAMES
        with type name = (Namectx1.Names.name, Namectx2.Names.name) Either.t) :
  TYPECTX
    with module Names = Names
     and type typ = (Namectx1.typ, Namectx2.typ) Either.t
     and type t = Namectx1.t * Namectx2.t = struct
  module Names = Names

  type typ = (Namectx1.typ, Namectx2.typ) Either.t
  type t = Namectx1.t * Namectx2.t [@@deriving to_yojson]

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
    (Names : Names.NAMES)
    (EmbedNames : sig
      val embed1 : Namectx1.Names.name -> Names.name
      val embed2 : Namectx2.Names.name -> Names.name
      val extract1 : Names.name -> Namectx1.Names.name option
      val extract2 : Names.name -> Namectx2.Names.name option
    end)
    (ClassifyTyp : sig
      val classify : Namectx1.typ -> bool
    end) :
  TYPECTX
    with module Names = Names
     and type typ = Namectx1.typ
     and type t = Namectx1.t * Namectx2.t = struct
  module Names = Names

  type typ = Namectx1.typ
  type t = Namectx1.t * Namectx2.t [@@deriving to_yojson]

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
