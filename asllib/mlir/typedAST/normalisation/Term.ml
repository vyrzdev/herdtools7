(* Constructor disambiguation is intentional; keep warning 42 nonfatal. *)
[@@@warnerror "-42"]

module Source = Asllib.AST
module Target = Representation.AST

type error = Common.error
type some_term = | Term : 'kind Target.term -> some_term [@@unboxed]

let (let*) = Result.bind

let kind_of_reference ~env name =
  match Environment.find_opt name env with
  | Some (Environment.Binding kind) -> Ok (Common.Kind kind)
  | None -> Error (Common.Violation 1)

let normalise_term_literal literal =
  match literal with
  | Source.L_Int value -> Ok(Term (Target.IntegerLiteral value : Target.integer Target.term))
  | _ -> Error Common.Unimplemented

let rec normalise_term ~env (expr: Source.expr) =
  match expr.Source.desc with
  | Source.E_Literal literal -> normalise_term_literal literal
  | Source.E_Var name ->
    let* Common.Kind kind = kind_of_reference ~env name in
    Ok (Term (Target.Reference (kind, name) : _ Target.term))
  | Source.E_Binop (op, left, right) -> normalise_term_binop ~env op left right
  | Source.E_Arbitrary _ -> Error Common.ArbitraryInAnnotation
  | _ -> Error Common.Unimplemented

and normalise_term_binop ~env op left right =
  match op with
  | `ADD -> normalise_term_binop_add ~env left right
  | `MUL -> normalise_term_binop_mul ~env left right
  | _ -> Error Common.Unimplemented

and normalise_term_binop_add ~env left right =
  let* Term left = normalise_term ~env left in
  let* Term right = normalise_term ~env right in
  match Target.kind_of_term left with
  | Target.Integer -> (
    (* An integer left operand requires an integer right operand. *)
    match Target.kind_of_term right with
    | Target.Integer -> (
      Ok (Term (Target.IntegerAdd (left, right) : Target.integer Target.term))
    )
  )

and normalise_term_binop_mul ~env left right =
  let* Term left = normalise_term ~env left in
  let* Term right = normalise_term ~env right in
  match Target.kind_of_term left with
  | Target.Integer -> (
    match Target.kind_of_term right with
    | Target.Integer ->
      Ok (Term (Target.IntegerMul (left, right) : Target.integer Target.term))
  )

let normalise_integer_constraint ~env = function
  | Source.Constraint_Exact expr -> (
    let* Term term = normalise_term ~env expr in
    match Target.kind_of_term term with
    | Target.Integer ->  Ok ((term, term) :
            Target.integer Target.term * Target.integer Target.term)
  )
  | Source.Constraint_Range (lower, upper) -> (
      let* Term lower = normalise_term ~env lower in
      let* Term upper = normalise_term ~env upper in
      match (Target.kind_of_term lower, Target.kind_of_term upper) with
      | (Target.Integer, Target.Integer) -> Ok ((lower, upper) :
              Target.integer Target.term * Target.integer Target.term)
  )

let rec normalise_integer_constraints ~env = function
  | [] -> Ok []
  | constraint_ :: rest ->
      let* range = normalise_integer_constraint ~env constraint_ in
      let* rest = normalise_integer_constraints ~env rest in
      Ok (range :: rest)

let normalise_integer_annotation ~env (annotation : Source.ty) :
    (Target.integer Target.typing_judgement, error) result =
  match annotation.Source.desc with
  | Source.T_Int constraint_kind -> (
    match constraint_kind with
    | Source.UnConstrained -> Ok (Target.IntegerUnconstrained)
    | Source.WellConstrained (constraints, precision) -> (
      match precision with
      | Source.Precision_Full ->
        let* constraints' = normalise_integer_constraints ~env constraints in
        Ok (Target.IntegerConstrained (constraints') : Target.integer Target.typing_judgement)

      (* TODO: For now, we defer precision lost type annotations - requires further review. *)
      | Source.Precision_Lost _ -> Error Common.PrecisionLoss
    )
    | Source.PendingConstrained -> Error (Common.Violation 2) (* Pending constraints should have been resolved by typing. *)
    | Source.Parameterized _ -> Error Common.Unimplemented
  )
  | Source.T_Named _ -> Error Common.Unimplemented
  | _ -> Error (Common.Violation 4) (* Got non-integer annotation on integer expression! *)
