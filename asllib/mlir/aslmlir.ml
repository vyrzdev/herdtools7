(* Collect input filenames, preserve command-line order *)
let parse_args () =
  let files = ref [] in
  let usage = "Usage: aslmlir FILE.asl [FILE.asl ...]" in
  Arg.parse [] (fun filename -> files := filename :: !files) usage;
  match List.rev !files with
  | [] ->
      Arg.usage [] usage;
      exit 2
  | files -> files

let run files =
  (* Combine the input files into one program before typing. *)
  let ast = List.concat_map (Asllib.Builder.from_file `ASLv1) files in

  (* Typecheck and annotate the AST *)
  let typed_ast, _ = Asllib.Typing.TypeCheckDefault.type_check_ast ast in

  (* Normalise the typed AST.t *)
  match Pipeline.run typed_ast with
  | Ok () -> 0
  | Error Pipeline.Unimplemented ->
      Format.eprintf "TypedAST normalisation is not implemented.@.";
      1

let () =
  let files = parse_args () in
  let status =
    try run files with
    | Asllib.Error.ASLException error ->
        Format.eprintf "%a@." Asllib.Error.pp_error error;
        1
    | Sys_error message ->
        Format.eprintf "%s@." message;
        1
  in
  exit status
