(* Constructor disambiguation is intentional; keep warning 42 nonfatal. *)
[@@@warnerror "-42"]

module Source = Asllib.AST
module Target = Representation.AST

type error = Common.error =
  | Violation of int
  | Unimplemented
  | PrecisionLoss
  | ArbitraryInAnnotation

type some_annotated_expr = | Expression : 'kind Target.annotated_expr -> some_annotated_expr [@@unboxed]
type some_source_type = | SourceType : 'kind Target.source_type -> some_source_type [@@unboxed]

let (let*) = Result.bind

let require_kind : type k other.
    k Target.kind -> other Target.annotated_expr ->
    (k Target.annotated_expr, error) result =
 fun kind expr ->
  match kind, Target.kind_of_expression expr with
  | Target.Integer, Target.Integer -> Ok expr

let normalise_expression_literal literal =
  match literal with
  | Source.L_Int value ->
    (* Drop annotation; Literals don't need annotations! They are intrinsic! *)
    Ok (Expression(Target.IntegerLiteral value))
  | _ -> Error Unimplemented

let normalise_expression_var ~env ~annotation name =
  match Environment.find_opt name env, annotation with
  | Some (Environment.Binding kind), Some annotation -> (
      match annotation.Source.desc with
      | Source.T_Int _ ->
          (* Typing copies the same across references; we rebuild it from the resulting source.
             Work and allocation grow with reference count * judgement size.
             We could also copy - for now we will convert the reference type annotation. *)
          let* judgement = Term.normalise_integer_annotation ~env annotation in
          assert (
            match kind, Target.kind_of_typing_judgement judgement with
            | Target.Integer, Target.Integer -> true);
          Ok (Expression (Target.Reference (judgement, name)))
      | _ -> Error Unimplemented
    )
  | None, _ | _, None -> Error (Violation 5)

let rec normalise_integer_source_constraint ~env = function
  | Source.Constraint_Exact expr -> (
      let* Expression expr = normalise_source_expression ~env expr in
      match Target.kind_of_expression expr with
      | Target.Integer ->
        Ok ((expr, expr) :
          Target.integer Target.annotated_expr * Target.integer Target.annotated_expr)
    )
  | Source.Constraint_Range (lower, upper) ->
      let* Expression lower = normalise_source_expression ~env lower in
      let* Expression upper = normalise_source_expression ~env upper in
      match Target.kind_of_expression lower with
      | Target.Integer -> (match Target.kind_of_expression upper with
        | Target.Integer ->
          Ok ((lower, upper) :
            Target.integer Target.annotated_expr * Target.integer Target.annotated_expr)
      )

and normalise_integer_source_constraints ~env = function
  | [] -> Ok []
  | constraint_ :: rest ->
      let* range = normalise_integer_source_constraint ~env constraint_ in
      let* rest = normalise_integer_source_constraints ~env rest in
      Ok (range :: rest)

and normalise_source_type ~env (ty : Source.ty) : (some_source_type, error) result =
  match ty.Source.desc with
  | Source.T_Int constraint_kind -> (
    match constraint_kind with
    | Source.UnConstrained -> Ok (SourceType Target.IntegerUnconstrained)
    | Source.WellConstrained (constraints, precision) -> (
      match precision with
      | Source.Precision_Full ->
        let* constraints = normalise_integer_source_constraints ~env constraints in
        Ok (SourceType (Target.IntegerConstrained constraints))
      | Source.Precision_Lost _ -> Error PrecisionLoss
    )
    | Source.PendingConstrained -> Error (Violation 6)
    | Source.Parameterized _ -> Error Unimplemented
  )
  | _ -> Error Unimplemented

and normalise_source_expression ~env (expr : Source.expr) =
  match expr.Source.desc with
    | Source.E_Literal literal -> normalise_expression_literal literal
    | Source.E_Var name ->
      normalise_expression_var ~env ~annotation:expr.Source.ty_opt name
    | Source.E_Binop (op, left, right) ->
      normalise_expression_binop ~env ~annotation:expr.Source.ty_opt op left right
    | Source.E_Arbitrary ty ->
      let* SourceType ty = normalise_source_type ~env ty in
      Ok (Expression (Target.Arbitrary ty))
    | _ -> Error Unimplemented

and normalise_expression_binop ~env ~annotation op left right =
  match op with
  | `ADD -> normalise_expression_binop_add ~env ~annotation left right
  | _ -> Error Unimplemented

and normalise_expression_binop_add ~env ~annotation left right =
  let* Expression left = normalise_source_expression ~env left in
  let* Expression right = normalise_source_expression ~env right in
  match Target.kind_of_expression left with
  | Target.Integer -> (
    (* An integer left operand requires an integer right operand. *)
    match Target.kind_of_expression right with
    | Target.Integer -> (
      match annotation with
      | Some ty ->
        let* judgement = Term.normalise_integer_annotation ~env ty in
        Ok (Expression (Target.IntegerAdd (judgement, left, right)))
      | None -> Error (Violation 7) (* Expected annotation on source expression after typing! *)
    )
  )
