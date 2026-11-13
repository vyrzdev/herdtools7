module Source = Asllib.AST
module Target = IR.AST
module Expression = NormaliseExpression
module Statement = NormaliseStatement

type some_arguments = Arguments : 'shape Target.arguments -> some_arguments [@@unboxed]

type some_domain = Expression.some_domain =
  | Domain : 'kind Target.domain * 'kind Expression.normaliser * 'kind Expression.domain_normaliser -> some_domain

type error =
  | Unsupported_pragma
  | Unsupported_getter
  | Unsupported_setter
  | Unsupported_primitive
  | Unsupported_config
  | Expression_error of Expression.error
  | Statement_error of Statement.error
  | Missing_parameter_type_annotation of Source.identifier
  | Unsupported_parameter_type of Source.identifier * Source.ty
  | Unsupported_argument_type of Source.identifier * Source.ty
  | Missing_return_type of Source.identifier
  | Unsupported_return_type of Source.ty
  | Missing_global_initial_value of Source.identifier
  | Unsupported_global_type of Source.identifier * Source.ty

let ( let* ) = Result.bind
let expression_result result = Result.map_error (fun error -> Expression_error error) result

let parameter ~env ~name ~ty_opt =
  match ty_opt with
  | None -> Error (Missing_parameter_type_annotation name)
  | Some ty ->
      let anonymous_ty = Asllib.Types.make_anonymous env ty in
      let* domain =
        match anonymous_ty.Source.desc with
        | Source.T_Int (Source.WellConstrained (constraints, _)) ->
            Expression.integer_domain_constraints ~env ~source:ty ~constraints |> expression_result
        | Source.T_Int (Source.Parameterized parameter_name) when String.equal name parameter_name ->
            (* Parameterized N names the supplied value; it imposes no constraint on that value. *)
            Ok (Target.IntegerDomain None)
        | _ -> Error (Unsupported_parameter_type (name, ty))
      in
      let result : Target.parameter = { Target.name = name; Target.domain = domain } in
      Ok result

let rec parameters ~env ~sources =
  match sources with
  | [] -> Ok []
  | (name, ty_opt) :: rest ->
      let* parameter = parameter ~env ~name ~ty_opt in
      let* rest = parameters ~env ~sources:rest in
      Ok (parameter :: rest)

let domain ~env ~on_unsupported ~source =
  Expression.domain ~env ~source
  |> Result.map_error (fun error ->
      match error with
      | Expression.Unsupported_type _ -> on_unsupported
      | Expression.Expression_error error -> Expression_error error)

let rec arguments ~env ~sources =
  match sources with
  | [] -> Ok (Arguments Target.NilArgument)
  | (name, ty) :: rest ->
      let* (Domain (domain, _, _)) = domain ~env ~on_unsupported:(Unsupported_argument_type (name, ty)) ~source:ty in
      let* (Arguments rest) = arguments ~env ~sources:rest in
      Ok (Arguments (Target.ConsArgument (name, domain, rest)))

let return_domain ~env ~source = domain ~env ~on_unsupported:(Unsupported_return_type source) ~source

let recurse_limit ~env ~source =
  match source with
  | None -> Ok None
  | Some source ->
      let* limit = Expression.integer_expression ~env source |> expression_result in
      Ok (Some limit)

let qualifier ~source =
  match source with
  | None -> None
  | Some Source.Pure -> Some Target.Pure
  | Some Source.Readonly -> Some Target.Readonly
  | Some Source.Noreturn -> Some Target.Noreturn

let body : type return_kind.
    env:Asllib.StaticEnv.env ->
    return_kind:return_kind Statement.return_kind ->
    source:Source.stmt ->
    (return_kind Target.statement, error) result =
 fun ~env ~return_kind ~source ->
  Statement.statement ~env ~return_kind source
  |> Result.map_error (fun error -> Statement_error error)

let function_declaration ~env ~(source : Source.func) ~body_source : (Target.declaration, error) result =
  let* parameters = parameters ~env ~sources:source.Source.parameters in
  let* (Arguments arguments) = arguments ~env ~sources:source.Source.args in
  let* (Domain (return_domain, return_expression, _)) =
    match source.Source.return_type with
    | Some ty -> return_domain ~env ~source:ty
    | None -> Error (Missing_return_type source.Source.name)
  in
  let* recurse_limit = recurse_limit ~env ~source:source.Source.recurse_limit in
  let qualifier = qualifier ~source:source.Source.qualifier in
  let* body = body ~env ~return_kind:(Statement.FunctionReturn return_expression) ~source:body_source in
  Ok
    (Target.Function
       { name = source.Source.name; parameters; arguments; return_domain; recurse_limit; qualifier; body })

let procedure_declaration ~env ~(source : Source.func) ~body_source : (Target.declaration, error) result =
  let* parameters = parameters ~env ~sources:source.Source.parameters in
  let* (Arguments arguments) = arguments ~env ~sources:source.Source.args in
  let* recurse_limit = recurse_limit ~env ~source:source.Source.recurse_limit in
  let qualifier = qualifier ~source:source.Source.qualifier in
  let* body = body ~env ~return_kind:Statement.ProcedureReturn ~source:body_source in
  Ok
    (Target.Procedure
       { name = source.Source.name; parameters; arguments; recurse_limit; qualifier; body })

let subprogram ~env ~(source : Source.func) : (Target.declaration, error) result =
  let env = Asllib.StaticEnv.with_empty_local env in
  match (source.Source.subprogram_type, source.Source.body) with
  | Source.ST_Getter, _ -> Error Unsupported_getter
  | Source.ST_Setter, _ -> Error Unsupported_setter
  | _, Source.SB_Primitive _ -> Error Unsupported_primitive
  | Source.ST_Function, Source.SB_ASL body_source -> function_declaration ~env ~source ~body_source
  | Source.ST_Procedure, Source.SB_ASL body_source -> procedure_declaration ~env ~source ~body_source

let initialised_global_storage ~(source : Source.global_decl) ~domain initial_value =
  let name = source.Source.name in
  match source.Source.keyword with
  | Source.GDK_Constant -> Ok (Target.GlobalConstant { name; domain; initial_value })
  | Source.GDK_Let -> Ok (Target.GlobalLet { name; domain; initial_value })
  | Source.GDK_Var -> Ok (Target.GlobalVariable { name; domain; initial_value })
  | Source.GDK_Config -> Error Unsupported_config

let global_storage ~env ~(source : Source.global_decl) : (Target.declaration, error) result =
  match source.Source.keyword with
  | Source.GDK_Config -> Error Unsupported_config
  | Source.GDK_Constant | Source.GDK_Let | Source.GDK_Var -> (
      let env = Asllib.StaticEnv.with_empty_local env in
      let name = source.Source.name in
      let* value =
        match source.Source.initial_value with
        | Some value -> Ok value
        | None -> Error (Missing_global_initial_value name)
      in
      (* Only a declared type supplies a domain; inferred facts stay in the initialiser. *)
      match source.Source.ty with
      | Some ty ->
          let* (Expression.Domain (domain, normalise_value, _)) =
            Expression.domain ~env ~source:ty
            |> Result.map_error (fun error ->
                match error with
                | Expression.Unsupported_type ty -> Unsupported_global_type (name, ty)
                | Expression.Expression_error error -> Expression_error error)
          in
          let* initial_value = normalise_value ~env value |> expression_result in
          initialised_global_storage ~source ~domain:(Some domain) initial_value
      | None ->
          let* ty = Expression.expression_type ~env value |> expression_result in
          let* (Expression.Normaliser (normalise_value, _)) =
            Expression.normaliser_for_type ~env ty
            |> Result.map_error (fun ty -> Unsupported_global_type (name, ty))
          in
          let* initial_value = normalise_value ~env value |> expression_result in
          initialised_global_storage ~source ~domain:None initial_value)

let declaration ~env ~(source : Source.decl) : (Target.declaration option, error) result =
  match source.Source.desc with
  | Source.D_Func func -> subprogram ~env ~source:func |> Result.map Option.some
  | Source.D_GlobalStorage storage -> global_storage ~env ~source:storage |> Result.map Option.some
  | Source.D_TypeDecl _ -> Ok None
  | Source.D_Pragma _ -> Error Unsupported_pragma

let rec run ~env ast =
  match ast with
  | [] -> Ok []
  | decl :: rest -> (
      match declaration ~env ~source:decl with
      | Error error -> Error error
      | Ok None -> run ~env rest
      | Ok (Some decl) -> ( match run ~env rest with Error error -> Error error | Ok rest -> Ok (decl :: rest)))
