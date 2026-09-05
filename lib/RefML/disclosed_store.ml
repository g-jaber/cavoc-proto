(* The store part of a move over RefML, see {!type: Nup.disclosed_store}. *)
module Make
    (AVal :
      Lang.Abstract_val.AVAL
        with type abstract_val = Nup.nup
         and type store = Store.store) :
  Lang.Disclosed_store.DISCLOSED_STORE
    with type store_ctx = Store.Storectx.t
     and type t = Nup.disclosed_store = struct
  type store_ctx = Store.Storectx.t
  type t = Nup.disclosed_store

  let pp_contents fmt contents =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    let pp_location fmt (level, nup) =
      Format.fprintf fmt "%a ↪ %a" Names.LocNames.pp_name level
        AVal.pp_abstract_val nup in
    Format.pp_print_list ~pp_sep pp_location fmt
      (List.mapi (fun i nup -> (i, nup)) contents)

  let pp fmt (ds : t) =
    Format.fprintf fmt
      "<@[<v>disclosed: %a@ locations: [@[<v>%a@]]@ pathdecl: [@[<v>%a@]]@ \
       pathcond: [@[<v>%a@]]@]>"
      Store.LocCtx.pp ds.local_locctx pp_contents ds.contents
      Symbolic.pp_pathdecl ds.symbolic_ctx.pathdecl Symbolic.pp_pathcond
      ds.symbolic_ctx.pathcond

  let to_string = Format.asprintf "%a" pp

  let to_yojson (ds : t) =
    `Assoc
      [
        ("disclosed", Store.LocCtx.to_yojson ds.local_locctx);
        ("locations", `List (List.map AVal.abstract_val_to_yojson ds.contents));
        ("symbolic_ctx", Symbolic.branch_to_yojson ds.symbolic_ctx);
      ]

  let empty : t =
    {
      local_locctx= Store.LocCtx.empty;
      contents= [];
      symbolic_ctx= Symbolic.empty;
      cons_ctx= Type_ctx.empty_cons_ctx;
    }

  let is_equiv ~compare_heaps (ds1 : t) (ds2 : t) =
    let constraints (ds : t) =
      { Store.empty_store with symbolic_ctx= ds.symbolic_ctx } in
    let is_equiv_content =
      AVal.is_equiv_abstract_val (constraints ds1) (constraints ds2) in
    ((not compare_heaps)
    || ds1.local_locctx = ds2.local_locctx
       && List.length ds1.contents = List.length ds2.contents
       && List.for_all2 is_equiv_content ds1.contents ds2.contents)
    && Util.Pmap.equal ds1.cons_ctx ds2.cons_ctx
end
