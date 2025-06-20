open MopsaLib

module type CPYTHON_IMPORTER =
sig
  val handle_import : string -> string -> range -> ('a, _) man -> 'a flow -> stmt * 'a flow
end

module Disabled_importer : CPYTHON_IMPORTER =
struct
  let handle_import _ _ range _ flow =
    panic "C disabled, no support for multilanguage C+Python analysis!"
end

let importer : (module CPYTHON_IMPORTER) option ref = ref None

let get_cpython_importer () =
  match !importer with
  | None -> Disabled_importer.handle_import
  | Some s ->
    let module D = (val s : CPYTHON_IMPORTER) in
    D.handle_import
