module Make (M : Util.Monad.BRANCH) = struct
  include Memory
  module M = M

  let generate_memory loc_ctx =
    Util.Debug.print_debug @@ "Generating memory for "
    ^ Type_ctx.string_of_loc_ctx loc_ctx;
    M.para_list
    @@ List.map (fun heap -> (Syntax.empty_val_env, heap)) (Heap.generate_heaps loc_ctx)
end