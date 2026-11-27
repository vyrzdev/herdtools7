module Source = Asllib.AST
module Target = Representation.AST

type _ return_kind =
  | Value : 'kind Target.kind -> 'kind return_kind
  | NoValue : Target.procedure_return return_kind

type error =
  | Violation of int
  | Unimplemented
  | Expression_error of Expression.error

let pp_error formatter = function
  | Violation reference -> Common.pp_error formatter (Common.Violation reference)
  | Unimplemented -> Common.pp_error formatter Common.Unimplemented
  | Expression_error error ->
      Format.fprintf formatter "Expression_error (%a)" Common.pp_error error

let (let*) = Result.bind

let normalise_statement_decl ~env keyword name source_type expr =
  let* Expression.Expression initialiser =
    Expression.normalise_source_expression ~env expr
    |> Result.map_error (fun error -> Expression_error error)
  in
  let declaration source_type initialiser =
    match keyword with
    | Source.LDK_Let -> Target.ImmutableDeclaration { name; source_type; initialiser }
    | Source.LDK_Var -> Target.MutableDeclaration { name; source_type; initialiser }
  in
  let* statement =
    match source_type with
    | None -> Ok (declaration None initialiser)
    | Some ty ->
        let* Expression.SourceType ty =
          Expression.normalise_source_type ~env ty
          |> Result.map_error (fun error -> Expression_error error)
        in
        let* initialiser =
          Expression.require_kind (Target.kind_of_source_type ty) initialiser
          |> Result.map_error (fun error -> Expression_error error)
        in
        Ok (declaration (Some ty) initialiser)
  in
  let binding = Environment.Binding (Target.kind_of_expression initialiser) in
  Ok (Environment.add name binding env, statement)

let rec normalise_statement : type k.
    env:Environment.t -> return_kind:k return_kind ->
    Source.stmt -> (Environment.t * k Target.statement, error) result =
 fun ~env ~return_kind stmt ->
  match stmt.Source.desc with
  | Source.S_Pass -> Ok (env, Target.Pass)
  | Source.S_Seq (first, second) ->
      let* env, first = normalise_statement ~env ~return_kind first in
      let* env, second = normalise_statement ~env ~return_kind second in
      Ok (env, Target.Sequence (first, second))
  | Source.S_Decl (keyword, Source.LDI_Var name, source_type, Some expr) ->
      normalise_statement_decl ~env keyword name source_type expr
  | Source.S_Return expr -> (
      match return_kind, expr with
      | Value kind, Some expr ->
          let* Expression.Expression expr =
            Expression.normalise_source_expression ~env expr
            |> Result.map_error (fun error -> Expression_error error)
          in
          let* expr =
            Expression.require_kind kind expr
            |> Result.map_error (fun error -> Expression_error error)
          in
          Ok (env, Target.Return expr)
      | NoValue, None -> Ok (env, Target.ProcedureReturn)
      | Value _, None | NoValue, Some _ -> Error (Violation 8) (* Function returns require a value; procedure returns must not carry one. *)
    )
  | _ -> Error Unimplemented
