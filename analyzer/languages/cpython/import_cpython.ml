open MopsaLib

module Enabled_importer : Python.Desugar.Import_cpython.CPYTHON_IMPORTER =
struct

  let handle_import probable_name filename range man flow =
    let () =
      C.Lang.Frontend.opt_make_target := probable_name in
    let prog = C.Lang.Frontend.parse_program [filename] in
    (* let () = debug "Parsed C program %a" pp_program prog in *)
    let () = C.Iterators.Program.Domain.opt_entry_function := "PyInit_" ^ probable_name in
    (* panic_at range "c.coverage hook mem: %b" (Hook.mem_hook "c.coverage"); *)
    let () = debug "Searching for entry function %s" !C.Iterators.Program.Domain.opt_entry_function
    (* (Format.pp_print_list (fun fmt fdec -> Format.pp_print_string fmt fdec.C.Ast.c_func_org_name)) (match prog.prog_kind with | C.Ast.C_program c -> c.c_functions | _ -> assert false) *)
    in
    let flow = C.Iterators.Program.Domain.init prog man flow |> Option.get |> post_to_flow man in
    let () =
      if Hook.is_hook_active "c.coverage" then
        C.Hooks.Coverage.Hook.init (Flow.get_ctx flow) in
    let body = mk_stmt (S_program(prog, None)) range in
    body, flow

end


let () = Python.Desugar.Import_cpython.importer := Some (module Enabled_importer)
