let () =
  let filename = ref None in
  let usage = "Usage: main.exe FILE.asl" in
  Arg.parse []
    (fun path ->
      match !filename with
      | None -> filename := Some path
      | Some _ -> raise (Arg.Bad "Expected one ASL file"))
    usage;
  match !filename with
  | None ->
      Arg.usage [] usage;
      exit 2
  | Some filename -> (
      try
        let ast = Asllib.Builder.from_file `ASLv1 filename in
        match Asl_mlir.Pipeline.run ast with
        | Ok _ -> ()
        | Error Asl_mlir.Normalise.Unsupported_pragma ->
            Format.eprintf "Pragmas are not supported.@.";
            exit 1
        | Error Asl_mlir.Normalise.Unsupported_getter ->
            Format.eprintf "Getters are not supported.@.";
            exit 1
        | Error Asl_mlir.Normalise.Unsupported_setter ->
            Format.eprintf "Setters are not supported.@.";
            exit 1
        | Error Asl_mlir.Normalise.Unsupported_primitive ->
            Format.eprintf "Primitives are not supported.@.";
            exit 1
        | Error Asl_mlir.Normalise.Unsupported_config ->
            Format.eprintf "Configuration globals are not supported.@.";
            exit 1
        | Error (Asl_mlir.Normalise.Expression_error (Asl_mlir.NormaliseExpression.Missing_type_annotation source))
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Expression_error (Asl_mlir.NormaliseExpression.Missing_type_annotation source))) ->
            Format.eprintf "Missing expression type annotation: %a.@." Asllib.PP.pp_expr source;
            exit 1
        | Error (Asl_mlir.Normalise.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_expression source))
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_expression source))) ->
            Format.eprintf "Unsupported expression: %a.@." Asllib.PP.pp_expr source;
            exit 1
        | Error (Asl_mlir.Normalise.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_call call))
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_call call))) ->
            Format.eprintf "Unsupported call to %s.@." call.Asllib.AST.name;
            exit 1
        | Error (Asl_mlir.Normalise.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_slice source))
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_slice source))) ->
            Format.eprintf "Unsupported slice: %a.@." Asllib.PP.pp_slice source;
            exit 1
        | Error (Asl_mlir.Normalise.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_domain source))
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Expression_error (Asl_mlir.NormaliseExpression.Unsupported_domain source))) ->
            Format.eprintf "Unsupported domain: %a.@." Asllib.PP.pp_ty source;
            exit 1
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Missing_target_type_annotation source)) ->
            Format.eprintf "Missing assignment target type annotation: %a.@." Asllib.PP.pp_lexpr source;
            exit 1
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Unsupported_target source)) ->
            Format.eprintf "Unsupported assignment target: %a.@." Asllib.PP.pp_lexpr source;
            exit 1
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Invalid_return source)) ->
            Format.eprintf "Invalid return: %a.@." Asllib.PP.pp_stmt source;
            exit 1
        | Error (Asl_mlir.Normalise.Statement_error (Asl_mlir.NormaliseStatement.Unsupported_statement source)) ->
            Format.eprintf "Unsupported statement: %a.@." Asllib.PP.pp_stmt source;
            exit 1
        | Error (Asl_mlir.Normalise.Missing_parameter_type_annotation name) ->
            Format.eprintf "Missing type annotation for parameter %s.@." name;
            exit 1
        | Error (Asl_mlir.Normalise.Unsupported_parameter_type (name, ty)) ->
            Format.eprintf "Unsupported type for parameter %s: %a.@." name Asllib.PP.pp_ty ty;
            exit 1
        | Error (Asl_mlir.Normalise.Unsupported_argument_type (name, ty)) ->
            Format.eprintf "Unsupported type for argument %s: %a.@." name Asllib.PP.pp_ty ty;
            exit 1
        | Error (Asl_mlir.Normalise.Missing_return_type name) ->
            Format.eprintf "Missing return type for function %s.@." name;
            exit 1
        | Error (Asl_mlir.Normalise.Unsupported_return_type ty) ->
            Format.eprintf "Unsupported return type: %a.@." Asllib.PP.pp_ty ty;
            exit 1
        | Error (Asl_mlir.Normalise.Missing_global_initial_value name) ->
            Format.eprintf "Missing initial value for global %s.@." name;
            exit 1
        | Error (Asl_mlir.Normalise.Unsupported_global_type (name, ty)) ->
            Format.eprintf "Unsupported type for global %s: %a.@." name Asllib.PP.pp_ty ty;
            exit 1
      with
      | Asllib.Error.ASLException error ->
          Format.eprintf "%a@." Asllib.Error.pp_error error;
          exit 1
      | Sys_error message ->
          Format.eprintf "%s@." message;
          exit 1)
