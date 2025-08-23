include
  Lang.Typectx.AggregateCommon (Fnamectx) (Pnamectx)
    (struct
      type t = Names.name

      let embed1 fn = Names.FName fn
      let embed2 pn = Names.PName pn

      let extract1 = function
        | Names.FName fn -> Some fn
        | Names.PName _ -> None

      let extract2 = function
        | Names.FName _ -> None
        | Names.PName pn -> Some pn
    end)
    (struct
      let classify = function
        | Types.TArrow _ | Types.TForall _ -> true
        | Types.TId _ | Types.TName _ -> false
        | _ -> failwith "Trying to add a name of the wrong type. Please report"
    end)

let build_name_ctx name_set =
  let (fname_set, pname_set) =
    List.partition_map
      (function Names.FName fn -> Left fn | Names.PName pn -> Right pn)
      name_set in
  let fresh_namectx_l name_set =
    List.map
      (fun n ->
        let tid = Types.fresh_typevar () in
        (n, tid))
      name_set in
  let fnamectx = Util.Pmap.list_to_pmap (fresh_namectx_l fname_set) in
  let pnamectx = Util.Pmap.list_to_pmap (fresh_namectx_l pname_set) in
  (fnamectx, pnamectx)
