module Make (BranchMonad : Util.Monad.BRANCH) = struct
  include Store
  module BranchMonad = BranchMonad


  let generate_store (loc_ctx, branch_ctx, cons_ctx) =
    Util.Debug.print_debug @@ "Generating store for "
    ^ Type_ctx.string_of_loc_ctx loc_ctx;
    let open BranchMonad in
    let branch = { Symbolic.empty with pathdecl = branch_ctx } in
    let* heap = BranchMonad.para_list @@ Heap.generate_heaps loc_ctx in
    return { empty_store with heap ; branch ; cons_ctx }
end
