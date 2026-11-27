[@@@warnerror "-42"]

module Source = Asllib.AST
module Target = Representation.AST

type error =
  | Violation of int
  | Unimplemented
  | Expression_error of Expression.error
  | Statement_error of Statement.error

let pp_error formatter = function
  | Violation reference -> Common.pp_error formatter (Common.Violation reference)
  | Unimplemented -> Common.pp_error formatter Common.Unimplemented
  | Expression_error error ->
      Format.fprintf formatter "Expression_error (%a)" Common.pp_error error
  | Statement_error error ->
      Format.fprintf formatter "Statement_error (%a)" Statement.pp_error error

let (let*) = Result.bind

let normalise_subprogram (func : Source.func) : (Target.declaration, error) result =
  (* These fields are not supported yet. Accept them only when empty;
     otherwise we would permit unsupported forms. *)
  let* () =
    match func.Source.parameters with
    | [] -> Ok ()
    | _ :: _ -> Error Unimplemented
  in
  let* () =
    match func.Source.args with
    | [] -> Ok ()
    | _ :: _ -> Error Unimplemented
  in
  let* () =
    match func.Source.recurse_limit with
    | None -> Ok ()
    | Some _ -> Error Unimplemented
  in
  let* () =
    match func.Source.qualifier with
    | None -> Ok ()
    | Some _ -> Error Unimplemented
  in
  let* () =
    match func.Source.override with
    | None -> Ok ()
    | Some _ -> Error Unimplemented
  in
  let env = Environment.empty in
  let name = func.Source.name in
  let* body =
    match func.Source.body with
    | Source.SB_ASL body -> Ok body
    | Source.SB_Primitive _ -> Error Unimplemented
  in
  match func.Source.subprogram_type, func.Source.return_type with
  | Source.ST_Function, Some return_type ->
      let* Expression.SourceType return_type =
        Expression.normalise_source_type ~env return_type
        |> Result.map_error (fun error -> Expression_error error)
      in
      let* _, body =
        Statement.normalise_statement ~env
          ~return_kind:(Statement.Value (Target.kind_of_source_type return_type)) body
        |> Result.map_error (fun error -> Statement_error error)
      in
      Ok (Target.Function { name; return_type; body })
  | Source.ST_Procedure, None ->
      let* _, body =
        Statement.normalise_statement ~env ~return_kind:Statement.NoValue body
        |> Result.map_error (fun error -> Statement_error error)
      in
      Ok (Target.Procedure { name; body })
  | (Source.ST_Getter | Source.ST_Setter), _ -> Error Unimplemented
  | Source.ST_Function, None | Source.ST_Procedure, Some _ -> Error (Violation 9) (* Functions must have a return type, Procedures must not. *)

let normalise_declaration (decl : Source.decl) =
  match decl.Source.desc with
  | Source.D_Func func -> normalise_subprogram func
  | _ -> Error Unimplemented
