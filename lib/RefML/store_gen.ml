module Make (M : Util.Monad.BRANCH) = struct
  include Store
  module M = M
  open M

  let generate_store (loc_ctx, cons_ctx) =
    Util.Debug.print_debug @@ "Generating store for "
    ^ Type_ctx.string_of_loc_ctx loc_ctx;
    let* heap = M.para_list @@ Heap.generate_heaps loc_ctx in
    return (Syntax.empty_val_env, heap, cons_ctx)
end
