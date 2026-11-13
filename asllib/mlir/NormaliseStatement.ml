module Source = Asllib.AST
module Target = IR.AST
module Expression = NormaliseExpression

type _ return_kind =
  | FunctionReturn : 'kind Expression.normaliser -> 'kind Target.function_return return_kind
  | ProcedureReturn : Target.procedure_return return_kind

type error =
  | Expression_error of Expression.error
  | Missing_target_type_annotation of Source.lexpr
  | Unsupported_target of Source.lexpr
  | Invalid_return of Source.stmt
  | Unsupported_statement of Source.stmt

type 'position some_assignment_target =
  | AssignmentTarget : ('kind, 'position, Target.allow_discard) Target.target * 'kind Expression.normaliser * 'kind Expression.domain_normaliser -> 'position some_assignment_target

type some_tuple_targets =
  | TupleTargets : 'shape Target.targets * 'shape Expression.component_normalisers -> some_tuple_targets

type some_declaration_binding =
  | DeclarationBinding : 'kind Target.binding * 'kind Expression.normaliser * 'kind Target.domain option -> some_declaration_binding

type some_tuple_bindings =
  | TupleBindings : 'shape Target.bindings * 'shape Expression.component_normalisers -> some_tuple_bindings

let ( let* ) = Result.bind
let expression_result result = Result.map_error (fun error -> Expression_error error) result

let loop_limit ~env = function
  | None -> Ok None
  | Some source ->
      let* limit = Expression.integer_expression ~env source |> expression_result in
      Ok (Some limit)

let rec storage_target : type kind position discard.
    env:Asllib.StaticEnv.env -> Source.lexpr -> ((kind, position, discard) Target.target, error) result =
 fun ~env source ->
  match source.Source.desc with
  | Source.LE_Var name -> Ok (Target.VariableTarget name)
  | Source.LE_SetArray (base, index) -> (
      let* ty =
        Expression.annotated_type ~env base |> Result.map_error (fun target -> Missing_target_type_annotation target)
      in
      match ty.Source.desc with
      | Source.T_Array _ ->
          let* base = storage_target ~env base in
          let* index = Expression.integer_expression ~env index |> expression_result in
          Ok (Target.ArrayElementTarget (base, index))
      | _ -> Error (Unsupported_target base))
  | Source.LE_SetField (base, field) -> (
      let* ty =
        Expression.annotated_type ~env base |> Result.map_error (fun target -> Missing_target_type_annotation target)
      in
      match ty.Source.desc with
      | Source.T_Record _ ->
          let* base = storage_target ~env base in
          Ok (Target.RecordFieldTarget (base, field))
      | _ -> Error (Unsupported_target base))
  | _ -> Error (Unsupported_target source)

let rec bitvector_target : type position discard.
    env:Asllib.StaticEnv.env -> Source.lexpr -> ((Target.bitvector, position, discard) Target.target, error) result =
 fun ~env source ->
  match source.Source.desc with
  | Source.LE_Slice (base, first_slice :: slices) -> (
      let* ty =
        Expression.annotated_type ~env base |> Result.map_error (fun target -> Missing_target_type_annotation target)
      in
      match ty.Source.desc with
      | Source.T_Bits _ ->
          let* base = bitvector_target ~env base in
          let* first_slice = Expression.slice ~env first_slice |> expression_result in
          let* slices = Expression.slices ~env slices |> expression_result in
          Ok (Target.BitvectorSliceTarget (base, first_slice, slices))
      | _ -> Error (Unsupported_target base))
  | _ -> storage_target ~env source

let assignment_target : type position.
    env:Asllib.StaticEnv.env -> ty:Source.ty -> Source.lexpr -> (position some_assignment_target, error) result =
 fun ~env ~ty source ->
  (* Callers resolve the outer type; tuple components are resolved individually. *)
  match (source.Source.desc, ty.Source.desc) with
  | Source.LE_Discard, _ ->
      let* (Expression.Normaliser (expression, domain)) =
        Expression.normaliser_for_type_desc ~env ~source:ty ty.Source.desc |> Result.map_error (fun _ -> Unsupported_target source)
      in
      Ok (AssignmentTarget (Target.DiscardTarget, expression, domain))
  | _, Source.T_Int _ ->
      let* target = storage_target ~env source in
      Ok (AssignmentTarget (target, Expression.integer_expression, Expression.integer_domain))
  | _, Source.T_Bool ->
      let* target = storage_target ~env source in
      Ok (AssignmentTarget (target, Expression.boolean_expression, Expression.boolean_domain))
  | _, Source.T_Bits _ ->
      let* target = bitvector_target ~env source in
      Ok (AssignmentTarget (target, Expression.bitvector_expression, Expression.bitvector_domain))
  | _, Source.T_Array (_, element_type) ->
      let* (Expression.Normaliser (element_expression, element_domain)) =
        Expression.normaliser_for_type ~env element_type |> Result.map_error (fun _ -> Unsupported_target source)
      in
      let* target = storage_target ~env source in
      Ok
        (AssignmentTarget
           ( target,
             Expression.array_expression ~element_expression ~element_domain,
             Expression.array_domain ~element_domain ))
  | _, Source.T_Tuple component_types ->
      let* (Expression.ComponentNormalisers components) =
        Expression.discover_components ~env component_types |> Result.map_error (fun _ -> Unsupported_target source)
      in
      let* target = storage_target ~env source in
      Ok
        (AssignmentTarget
           (target, Expression.tuple_expression ~components, Expression.tuple_domain ~components))
  | _, Source.T_Record _ ->
      let* target = storage_target ~env source in
      Ok (AssignmentTarget (target, Expression.record_expression, Expression.record_domain))
  | _ -> Error (Unsupported_target source)

let rec tuple_targets ~env ~source types sources =
  match (types, sources) with
  | [], [] -> Ok (TupleTargets (Target.NilTarget, Expression.NilNormaliser))
  | ty :: types, target :: sources ->
      let ty = Asllib.Types.make_anonymous env ty in
      let* (AssignmentTarget (target, expression, domain)) = assignment_target ~env ~ty target in
      let* (TupleTargets (targets, components)) = tuple_targets ~env ~source types sources in
      Ok (TupleTargets (Target.ConsTarget (target, targets), Expression.ConsNormaliser (expression, domain, components)))
  | _ -> Error (Unsupported_target source)

let rec record_field_slices ~source ~fields ~slices =
  match (fields, slices) with
  | [], [] -> Ok []
  | field :: fields, (offset, length) :: slices ->
      let slice : Target.record_field_slice = Target.{ field; offset; length } in
      let* slices = record_field_slices ~source ~fields ~slices in
      Ok (slice :: slices)
  | _ -> Error (Unsupported_target source)

(* TODO: Call pattern/flow re: assignment target normalisation is suspect... *)
let assignment ~env (source : Source.lexpr) value =
  let* ty =
    Expression.annotated_type ~env source |> Result.map_error (fun target -> Missing_target_type_annotation target)
  in
  match source.Source.desc with
  | Source.LE_Destructuring sources -> (
      match ty.Source.desc with
      | Source.T_Tuple component_types -> (
          let* (TupleTargets (targets, components)) = tuple_targets ~env ~source component_types sources in
          match targets with
          | (Target.ConsTarget (_, Target.ConsTarget (_, _)) as targets) ->
              let* value = Expression.tuple_expression ~env ~components value |> expression_result in
              Ok (Target.Assignment (Target.TupleTarget targets, value))
          | _ -> Error (Unsupported_target source))
      | _ -> Error (Unsupported_target source))
  | Source.LE_SetFields
      (base, first :: second :: fields, (first_offset, first_length) :: (second_offset, second_length) :: slices) -> (
      let* base_ty =
        Expression.annotated_type ~env base |> Result.map_error (fun target -> Missing_target_type_annotation target)
      in
      match (base.Source.desc, base_ty.Source.desc) with
      | Source.LE_Var name, Source.T_Record _ ->
          let first : Target.record_field_slice = Target.{ field = first; offset = first_offset; length = first_length } in
          let second : Target.record_field_slice = Target.{ field = second; offset = second_offset; length = second_length } in
          let* fields = record_field_slices ~source ~fields ~slices in
          let* value = Expression.bitvector_expression ~env value |> expression_result in
          Ok (Target.Assignment (Target.RecordFieldsTarget (name, first, second, fields), value))
      | _ -> Error (Unsupported_target base))
  | _ ->
      let* (AssignmentTarget (target, normalise_value, _)) = assignment_target ~env ~ty source in
      let* value = normalise_value ~env value |> expression_result in
      Ok (Target.Assignment (target, value))

let rec tuple_bindings ~env ~source types names =
  match (types, names) with
  | [], [] -> Ok (TupleBindings (Target.NilBinding, Expression.NilNormaliser))
  | ty :: types, name :: names ->
      let* (Expression.Normaliser (expression, domain)) =
        Expression.normaliser_for_type ~env ty |> Result.map_error (fun _ -> Unsupported_statement source)
      in
      let* (TupleBindings (bindings, components)) = tuple_bindings ~env ~source types names in
      Ok (TupleBindings (Target.ConsBinding (name, bindings), Expression.ConsNormaliser (expression, domain, components)))
  | _ -> Error (Unsupported_statement source)

let rec tuple_bindings_from_domains : type shape.
    source:Source.stmt -> shape Target.domains -> Source.identifier list -> (shape Target.bindings, error) result =
 fun ~source domains names ->
  match (domains, names) with
  | Target.NilDomain, [] -> Ok Target.NilBinding
  | Target.ConsDomain (_, domains), name :: names ->
      let* bindings = tuple_bindings_from_domains ~source domains names in
      Ok (Target.ConsBinding (name, bindings))
  | _ -> Error (Unsupported_statement source)

let binding_from_domain : type kind.
    source:Source.stmt -> kind Target.domain -> Source.local_decl_item -> (kind Target.binding, error) result =
 fun ~source domain item ->
  match (item, domain) with
  | Source.LDI_Var name, _ -> Ok (Target.VariableBinding name)
  | Source.LDI_Tuple names, Target.TupleDomain domains ->
      let* bindings = tuple_bindings_from_domains ~source domains names in
      Ok (Target.TupleBinding bindings)
  | _ -> Error (Unsupported_statement source)

let declaration_binding ~env ~source ~ty_opt ~value item =
  (* Retain a declared domain. Otherwise, the initialiser supplies kind/shape;
     its computed domain is not an additional contract to preserve. *)
  match ty_opt with
  | Some ty ->
      let* (Expression.Domain (domain, expression, _)) =
        Expression.domain ~env ~source:ty
        |> Result.map_error (fun error ->
            match error with
            | Expression.Unsupported_type _ -> Unsupported_statement source
            | Expression.Expression_error error -> Expression_error error)
      in
      let* binding = binding_from_domain ~source domain item in
      Ok (DeclarationBinding (binding, expression, Some domain))
  | None -> (
      let* ty = Expression.expression_type ~env value |> expression_result in
      match item with
      | Source.LDI_Var name ->
          let* (Expression.Normaliser (expression, _)) =
            Expression.normaliser_for_type_desc ~env ~source:ty ty.Source.desc |> Result.map_error (fun _ -> Unsupported_statement source)
          in
          Ok (DeclarationBinding (Target.VariableBinding name, expression, None))
      | Source.LDI_Tuple names -> (
          match ty.Source.desc with
          | Source.T_Tuple types -> (
              let* (TupleBindings (bindings, components)) = tuple_bindings ~env ~source types names in
              match bindings with
              | (Target.ConsBinding (_, Target.ConsBinding (_, _)) as bindings) ->
                  Ok (DeclarationBinding (Target.TupleBinding bindings, Expression.tuple_expression ~components, None))
              | _ -> Error (Unsupported_statement source))
          | _ -> Error (Unsupported_statement source)))

let rec statement : type kind.
    env:Asllib.StaticEnv.env -> return_kind:kind return_kind -> Source.stmt -> (kind Target.statement, error) result =
 fun ~env ~return_kind source ->
  match source.Source.desc with
  | Source.S_Pass -> Ok Target.Pass
  | Source.S_Seq (first, second) ->
      let* first = statement ~env ~return_kind first in
      let* second = statement ~env ~return_kind second in
      Ok (Target.Sequence (first, second))
  | Source.S_Decl (keyword, item, ty_opt, Some value) ->
      let* (DeclarationBinding (binding, normalise_value, domain)) =
        declaration_binding ~env ~source ~ty_opt ~value item
      in
      let* initial_value = normalise_value ~env value |> expression_result in
      (match keyword with
      | Source.LDK_Var -> Ok (Target.MutableDeclaration { binding; domain; initial_value })
      | Source.LDK_Let -> Ok (Target.ImmutableDeclaration { binding; domain; initial_value }))
  | Source.S_Assign
      (* TODO: Should we really enumerate all this? Isn't it fine? *)
      ( ({ Source.desc = Source.LE_Discard | Source.LE_Var _ | Source.LE_SetArray _ | Source.LE_Slice _ | Source.LE_SetField _ | Source.LE_SetFields _ | Source.LE_Destructuring _; _ } as target),
        value ) ->
      assignment ~env target value
  | Source.S_Call call -> (
      match call.Source.call_type with
      | Source.ST_Procedure ->
          let* parameters = Expression.call_parameters ~env call.Source.params |> expression_result in
          let* (Expression.Expressions arguments) = Expression.call_arguments ~env call |> expression_result in
          Ok (Target.ProcedureCall { name = call.Source.name; parameters = parameters; arguments = arguments })
      | Source.ST_Function | Source.ST_Getter | Source.ST_Setter -> Error (Unsupported_statement source))
  | Source.S_Assert condition ->
      let* condition = Expression.boolean_expression ~env condition |> expression_result in
      Ok (Target.Assert condition)
  | Source.S_Unreachable -> Ok Target.Unreachable
  | Source.S_Return returned_value -> (
      match (return_kind, returned_value) with
      | ProcedureReturn, None -> Ok Target.ProcedureReturn
      | FunctionReturn expression, Some value ->
          let* value = expression ~env value |> expression_result in
          Ok (Target.Return value)
      | ProcedureReturn, Some _ -> Error (Invalid_return source)
      | FunctionReturn _, None -> Error (Invalid_return source))
  | Source.S_Cond (condition, then_branch, else_branch) ->
      let* condition = Expression.boolean_expression ~env condition |> expression_result in
      let* then_branch = statement ~env ~return_kind then_branch in
      let* else_branch = statement ~env ~return_kind else_branch in
      Ok (Target.Conditional { condition; then_branch; else_branch })
  | Source.S_While (condition, limit, body) ->
      let* condition = Expression.boolean_expression ~env condition |> expression_result in
      let* limit = loop_limit ~env limit in
      let* body = statement ~env ~return_kind body in
      Ok (Target.While { condition; body; limit })
  | Source.S_Repeat (body, until_condition, limit) ->
      let* body = statement ~env ~return_kind body in
      let* until_condition = Expression.boolean_expression ~env until_condition |> expression_result in
      let* limit = loop_limit ~env limit in
      Ok (Target.Repeat { body; until_condition; limit })
  | Source.S_For { index_name; start_e; dir; end_e; body; limit } ->
      let* start = Expression.integer_expression ~env start_e |> expression_result in
      let direction = match dir with Source.Up -> Target.Up | Source.Down -> Target.Down in
      let* end_ = Expression.integer_expression ~env end_e |> expression_result in
      let* limit = loop_limit ~env limit in
      let* body = statement ~env ~return_kind body in
      Ok (Target.For { index_name; start; direction; end_; body; limit })
  | _ -> Error (Unsupported_statement source)
