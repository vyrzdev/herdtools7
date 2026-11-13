module Source = Asllib.AST
module Target = IR.AST

type error =
  | Missing_type_annotation of Source.expr
  | Unsupported_expression of Source.expr
  | Unsupported_call of Source.call
  | Unsupported_slice of Source.slice
  | Unsupported_domain of Source.ty

(* A 'kind normaliser takes an environment, and a source expression and produces a 'kind expression or an error. *)
type 'kind normaliser = env:Asllib.StaticEnv.env -> Source.expr -> ('kind Target.expression, error) result

type 'kind domain_normaliser = env:Asllib.StaticEnv.env -> source:Source.ty -> ('kind Target.domain, error) result

type _ component_normalisers =
  | NilNormaliser : unit component_normalisers
  | ConsNormaliser : 'kind normaliser * 'kind domain_normaliser * 'rest component_normalisers -> ('kind * 'rest) component_normalisers

type some_normaliser = Normaliser : 'kind normaliser * 'kind domain_normaliser -> some_normaliser

type some_domain = Domain : 'kind Target.domain * 'kind normaliser * 'kind domain_normaliser -> some_domain

type any_domain = AnyDomain : 'kind Target.domain -> any_domain [@@unboxed]

type domain_error =
  | Unsupported_type of Source.ty
  | Expression_error of error

type some_domains = Domains : 'shape Target.domains * 'shape component_normalisers -> some_domains

type any_domains = AnyDomains : 'shape Target.domains -> any_domains [@@unboxed]

type some_component_normalisers = ComponentNormalisers : 'shape component_normalisers -> some_component_normalisers [@@unboxed]

type some_expressions = Expressions : 'shape Target.expressions -> some_expressions [@@unboxed]

type 'kind tuple_selection = TupleSelection : 'shape component_normalisers * ('shape, 'kind) Target.tuple_position -> 'kind tuple_selection

let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | value :: rest ->
      let* value = f value in
      let* rest = map_result f rest in
      Ok (value :: rest)

let annotated_type ~env source =
  match source.Source.ty_opt with Some ty -> Ok (Asllib.Types.make_anonymous env ty) | None -> Error source

let expression_type ~env (expression : Source.expr) =
  annotated_type ~env expression |> Result.map_error (fun source -> Missing_type_annotation source)

let domain_expression_result result = Result.map_error (fun error -> Expression_error error) result

let rec integer_expression ~env (source : Source.expr) : (Target.integer Target.expression, error) result =
  (* Typed positions or prior inspection select this function; do not recheck the kind on entry.
     Literals reveal their kind. Check references and call results explicitly: their child
     expressions do not establish the result kind, and zero-argument calls have no children.
     New constructors must expose a kind mismatch or check it; eventual failure is not automatic.
     Keep operand inspection for overload selection. ATCs carry a target type; handle them separately. *)
  match source.Source.desc with
  | Source.E_Literal (Source.L_Int value) -> Ok (Target.IntegerLiteral value)
  | Source.E_Arbitrary ty ->
      let* domain = integer_domain ~env ~source:ty in
      Ok (Target.Arbitrary domain)
  | Source.E_ATC (value, ty) ->
      let* domain = integer_domain ~env ~source:ty in
      let* value = integer_expression ~env value in
      Ok (Target.IntegerAssertedConversion (value, domain))
  | Source.E_Var name -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Int _ -> Ok (Target.Reference name)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Call call -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Int _ -> function_call ~env ~source call
      | _ -> Error (Unsupported_expression source))
  | Source.E_Unop (op, operand) -> (
      let* operand_type = expression_type ~env operand in
      match (op, operand_type.Source.desc) with
      | Source.NEG, Source.T_Int _ ->
          let* operand = integer_expression ~env operand in
          Ok (Target.IntegerNegate operand)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Binop (op, left, right) -> (
      let* left_type = expression_type ~env left in
      let* right_type = expression_type ~env right in
      match (op, left_type.Source.desc, right_type.Source.desc) with
      | `ADD, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerAdd (left, right))
      | `SUB, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerSubtract (left, right))
      | `MUL, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerMultiply (left, right))
      | `DIV, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerExactDivide (left, right))
      | `DIVRM, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerFloorDivide (left, right))
      | `MOD, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerRemainder (left, right))
      | `POW, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerPower (left, right))
      | `SHL, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerShiftLeft (left, right))
      | `SHR, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerShiftRight (left, right))
      | _ -> Error (Unsupported_expression source))
  | Source.E_Cond (condition, then_branch, else_branch) ->
      let* condition = boolean_expression ~env condition in
      let* then_branch = integer_expression ~env then_branch in
      let* else_branch = integer_expression ~env else_branch in
      Ok (Target.ConditionalExpression { condition; then_branch; else_branch })
  | Source.E_GetArray (base, index) ->
      let* base = array_expression ~env ~element_expression:integer_expression ~element_domain:integer_domain base in
      let* index = integer_expression ~env index in
      Ok (Target.ArrayAccess (base, index))
  | Source.E_GetItem (base, index) ->
      let* (TupleSelection (components, position)) =
        tuple_selection ~env ~source base index ~select:(fun ty ->
            match ty.Source.desc with
            | Source.T_Int _ -> Ok (integer_expression, integer_domain)
            | _ -> Error (Unsupported_expression source))
      in
      let* base = tuple_expression ~env ~components base in
      Ok (Target.TupleAccess (base, position))
  | Source.E_GetField (base, name) ->
      let* base = record_expression ~env base in
      Ok (Target.RecordAccess (base, name))
  | _ -> Error (Unsupported_expression source)

and bitvector_expression ~env (source : Source.expr) : (Target.bitvector Target.expression, error) result =
  match source.Source.desc with
  | Source.E_Literal (Source.L_BitVector value) -> Ok (Target.BitvectorLiteral value)
  | Source.E_Arbitrary ty ->
      let* domain = bitvector_domain ~env ~source:ty in
      Ok (Target.Arbitrary domain)
  | Source.E_ATC (value, ty) ->
      let* domain = bitvector_domain ~env ~source:ty in
      let* value = bitvector_expression ~env value in
      Ok (Target.BitvectorAssertedConversion (value, domain))
  | Source.E_Var name -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Bits _ -> Ok (Target.Reference name)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Call call -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Bits _ -> function_call ~env ~source call
      | _ -> Error (Unsupported_expression source))
  | Source.E_Unop (op, operand) -> (
      let* operand_type = expression_type ~env operand in
      match (op, operand_type.Source.desc) with
      | Source.NOT, Source.T_Bits _ ->
          let* operand = bitvector_expression ~env operand in
          Ok (Target.BitvectorNot operand)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Binop (op, left, right) -> (
      let* left_type = expression_type ~env left in
      let* right_type = expression_type ~env right in
      match (op, left_type.Source.desc, right_type.Source.desc) with
      | `AND, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorAnd (left, right))
      | `OR, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorOr (left, right))
      | `XOR, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorXor (left, right))
      | `BV_CONCAT, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorConcat (left, right))
      | `ADD, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorAdd (left, right))
      | `SUB, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorSubtract (left, right))
      | `ADD, Source.T_Bits _, Source.T_Int _ ->
          let* left = bitvector_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.BitvectorIntegerAdd (left, right))
      | `SUB, Source.T_Bits _, Source.T_Int _ ->
          let* left = bitvector_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.BitvectorIntegerSubtract (left, right))
      | _ -> Error (Unsupported_expression source))
  | Source.E_Slice (base, first_slice :: source_slices) -> (
      let* base_type = expression_type ~env base in
      match base_type.Source.desc with
      | Source.T_Int _ ->
          let* base = integer_expression ~env base in
          let* first_slice = slice ~env first_slice in
          let* slices = slices ~env source_slices in
          Ok (Target.IntegerSlice (base, first_slice, slices))
      | Source.T_Bits _ ->
          let* base = bitvector_expression ~env base in
          let* first_slice = slice ~env first_slice in
          let* slices = slices ~env source_slices in
          Ok (Target.BitvectorSlice (base, first_slice, slices))
      | _ -> Error (Unsupported_expression source))
  | Source.E_Cond (condition, then_branch, else_branch) ->
      let* condition = boolean_expression ~env condition in
      let* then_branch = bitvector_expression ~env then_branch in
      let* else_branch = bitvector_expression ~env else_branch in
      Ok (Target.ConditionalExpression { condition; then_branch; else_branch })
  | Source.E_GetArray (base, index) ->
      let* base = array_expression ~env ~element_expression:bitvector_expression ~element_domain:bitvector_domain base in
      let* index = integer_expression ~env index in
      Ok (Target.ArrayAccess (base, index))
  | Source.E_GetItem (base, index) ->
      let* (TupleSelection (components, position)) =
        tuple_selection ~env ~source base index ~select:(fun ty ->
            match ty.Source.desc with
            | Source.T_Bits _ -> Ok (bitvector_expression, bitvector_domain)
            | _ -> Error (Unsupported_expression source))
      in
      let* base = tuple_expression ~env ~components base in
      Ok (Target.TupleAccess (base, position))
  | Source.E_GetField (base, name) ->
      let* base = record_expression ~env base in
      Ok (Target.RecordAccess (base, name))
  | Source.E_GetFields (base, first :: fields) ->
      let* base = record_expression ~env base in
      Ok (Target.RecordFieldsAccess (base, first, fields))
  | _ -> Error (Unsupported_expression source)

and boolean_expression ~env (source : Source.expr) : (Target.boolean Target.expression, error) result =
  match source.Source.desc with
  | Source.E_Literal (Source.L_Bool value) -> Ok (Target.BooleanLiteral value)
  | Source.E_Arbitrary ty ->
      let* domain = boolean_domain ~env ~source:ty in
      Ok (Target.Arbitrary domain)
  | Source.E_Var name -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Bool -> Ok (Target.Reference name)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Call call -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Bool -> function_call ~env ~source call
      | _ -> Error (Unsupported_expression source))
  | Source.E_Unop (op, operand) -> (
      let* operand_type = expression_type ~env operand in
      match (op, operand_type.Source.desc) with
      | Source.BNOT, Source.T_Bool ->
          let* operand = boolean_expression ~env operand in
          Ok (Target.BooleanNot operand)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Binop (op, left, right) -> (
      let* left_type = expression_type ~env left in
      let* right_type = expression_type ~env right in
      match (op, left_type.Source.desc, right_type.Source.desc) with
      | `BAND, Source.T_Bool, Source.T_Bool ->
          let* left = boolean_expression ~env left in
          let* right = boolean_expression ~env right in
          Ok (Target.BooleanAnd (left, right))
      | `BOR, Source.T_Bool, Source.T_Bool ->
          let* left = boolean_expression ~env left in
          let* right = boolean_expression ~env right in
          Ok (Target.BooleanOr (left, right))
      | `IMPL, Source.T_Bool, Source.T_Bool ->
          let* left = boolean_expression ~env left in
          let* right = boolean_expression ~env right in
          Ok (Target.BooleanImplies (left, right))
      | `BEQ, Source.T_Bool, Source.T_Bool ->
          let* left = boolean_expression ~env left in
          let* right = boolean_expression ~env right in
          Ok (Target.BooleanEquivalent (left, right))
      | `EQ, Source.T_Bool, Source.T_Bool ->
          let* left = boolean_expression ~env left in
          let* right = boolean_expression ~env right in
          Ok (Target.BooleanEqual (left, right))
      | `NE, Source.T_Bool, Source.T_Bool ->
          let* left = boolean_expression ~env left in
          let* right = boolean_expression ~env right in
          Ok (Target.BooleanNotEqual (left, right))
      | `EQ, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerEqual (left, right))
      | `NE, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerNotEqual (left, right))
      | `LT, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerLessThan (left, right))
      | `LE, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerLessOrEqual (left, right))
      | `GT, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerGreaterThan (left, right))
      | `GE, Source.T_Int _, Source.T_Int _ ->
          let* left = integer_expression ~env left in
          let* right = integer_expression ~env right in
          Ok (Target.IntegerGreaterOrEqual (left, right))
      | `EQ, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorEqual (left, right))
      | `NE, Source.T_Bits _, Source.T_Bits _ ->
          let* left = bitvector_expression ~env left in
          let* right = bitvector_expression ~env right in
          Ok (Target.BitvectorNotEqual (left, right))
      | _ -> Error (Unsupported_expression source))
  | Source.E_Cond (condition, then_branch, else_branch) ->
      let* condition = boolean_expression ~env condition in
      let* then_branch = boolean_expression ~env then_branch in
      let* else_branch = boolean_expression ~env else_branch in
      Ok (Target.ConditionalExpression { condition; then_branch; else_branch })
  | Source.E_GetArray (base, index) ->
      let* base = array_expression ~env ~element_expression:boolean_expression ~element_domain:boolean_domain base in
      let* index = integer_expression ~env index in
      Ok (Target.ArrayAccess (base, index))
  | Source.E_GetItem (base, index) ->
      let* (TupleSelection (components, position)) =
        tuple_selection ~env ~source base index ~select:(fun ty ->
            match ty.Source.desc with
            | Source.T_Bool -> Ok (boolean_expression, boolean_domain)
            | _ -> Error (Unsupported_expression source))
      in
      let* base = tuple_expression ~env ~components base in
      Ok (Target.TupleAccess (base, position))
  | Source.E_GetField (base, name) ->
      let* base = record_expression ~env base in
      Ok (Target.RecordAccess (base, name))
  | _ -> Error (Unsupported_expression source)

and slices ~env sources = map_result (slice ~env) sources

and slice ~env = function
  | Source.Slice_Length (offset, length) ->
      let* offset = integer_expression ~env offset in
      let* length = integer_expression ~env length in
      Ok Target.{ offset; length }
  | source -> Error (Unsupported_slice source)

(* Arrays can have arbitrary element type/kind *)
(* Some* callers will know the element type/kind at the callsite...  *)
(* consider array access integer_expression, base is integer array *)
(* Thus, let caller pass element expression and domain functions! *)
(* Unknown cases resolved on the callers end! *)
and array_expression : type element.
    env:Asllib.StaticEnv.env ->
    element_expression:element normaliser ->
    element_domain:element domain_normaliser ->
    Source.expr ->
    (element Target.array_kind Target.expression, error) result =
 fun ~env ~element_expression ~element_domain source ->
  match source.Source.desc with
  | Source.E_Arbitrary ty ->
      let* domain = array_domain ~env ~element_domain ~source:ty in
      Ok (Target.Arbitrary domain)
  | Source.E_Array { length; value } ->
      let* length = integer_expression ~env length in
      let* value = element_expression ~env value in
      Ok (Target.ArrayCreate { length; value })
  | Source.E_Var name -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      (* The caller establishes the element kind. *)
      | Source.T_Array _ -> Ok (Target.Reference name)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Call call -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Array _ -> function_call ~env ~source call
      | _ -> Error (Unsupported_expression source))
  | Source.E_Cond (condition, then_branch, else_branch) ->
      let* condition = boolean_expression ~env condition in
      let* then_branch = array_expression ~env ~element_expression ~element_domain then_branch in
      let* else_branch = array_expression ~env ~element_expression ~element_domain else_branch in
      Ok (Target.ConditionalExpression { condition; then_branch; else_branch })
  | Source.E_GetArray (base, index) ->
      let* base =
        array_expression ~env
          ~element_expression:(array_expression ~element_expression ~element_domain)
          ~element_domain:(array_domain ~element_domain) base
      in
      let* index = integer_expression ~env index in
      Ok (Target.ArrayAccess (base, index))
  | Source.E_GetItem (base, index) ->
      let* (TupleSelection (components, position)) =
        tuple_selection ~env ~source base index ~select:(fun ty ->
            match ty.Source.desc with
            | Source.T_Array _ -> Ok (array_expression ~element_expression ~element_domain, array_domain ~element_domain)
            | _ -> Error (Unsupported_expression source))
      in
      let* base = tuple_expression ~env ~components base in
      Ok (Target.TupleAccess (base, position))
  | Source.E_GetField (base, name) ->
      let* base = record_expression ~env base in
      Ok (Target.RecordAccess (base, name))
  | _ -> Error (Unsupported_expression source)

and tuple_expression : type shape.
    env:Asllib.StaticEnv.env ->
    components:shape component_normalisers ->
    Source.expr ->
    (shape Target.tuple_kind Target.expression, error) result =
 fun ~env ~components source ->
  match source.Source.desc with
  | Source.E_Tuple operands -> (
      let* elements = expressions ~env ~on_mismatch:(Unsupported_expression source) components operands in
      match elements with
      | (Target.Cons (_, Target.Cons (_, _)) as elements) -> Ok (Target.TupleConstructor elements : shape Target.tuple_kind Target.expression)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Arbitrary ty ->
      let* domain = tuple_domain ~env ~components ~source:ty in
      Ok (Target.Arbitrary domain)
  | Source.E_ATC (value, ty) ->
      let* domain = tuple_domain ~env ~components ~source:ty in
      let* value = tuple_expression ~env ~components value in
      Ok (Target.TupleAssertedConversion (value, domain))
  | Source.E_Var name -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Tuple _ -> Ok (Target.Reference name)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Call call -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Tuple _ -> function_call ~env ~source call
      | _ -> Error (Unsupported_expression source))
  | Source.E_Cond (condition, then_branch, else_branch) ->
      let* condition = boolean_expression ~env condition in
      let* then_branch = tuple_expression ~env ~components then_branch in
      let* else_branch = tuple_expression ~env ~components else_branch in
      Ok (Target.ConditionalExpression { condition; then_branch; else_branch })
  | Source.E_GetArray (base, index) ->
      let* base =
        array_expression ~env ~element_expression:(tuple_expression ~components) ~element_domain:(tuple_domain ~components) base
      in
      let* index = integer_expression ~env index in
      Ok (Target.ArrayAccess (base, index))
  | Source.E_GetItem (base, index) ->
      let* (TupleSelection (base_components, position)) =
        tuple_selection ~env ~source base index ~select:(fun ty ->
            match ty.Source.desc with
            | Source.T_Tuple _ -> Ok (tuple_expression ~components, tuple_domain ~components)
            | _ -> Error (Unsupported_expression source))
      in
      let* base = tuple_expression ~env ~components:base_components base in
      Ok (Target.TupleAccess (base, position))
  | Source.E_GetField (base, name) ->
      let* base = record_expression ~env base in
      Ok (Target.RecordAccess (base, name))
  | _ -> Error (Unsupported_expression source)

and record_expression ~env (source : Source.expr) : (Target.record_kind Target.expression, error) result =
  match source.Source.desc with
  | Source.E_Record (ty, fields) -> (
      match (Asllib.Types.make_anonymous env ty).Source.desc with
      | Source.T_Record field_types ->
          let field_expression (name, value) =
            match List.assoc_opt name field_types with
            | Some ty ->
                let* (Normaliser (convert, _)) =
                  normaliser_for_type ~env ty |> Result.map_error (fun _ -> Unsupported_expression value)
                in
                let* value = convert ~env value in
                Ok (Target.RecordField (name, value))
            | None -> Error (Unsupported_expression source)
          in
          let* fields = map_result field_expression fields in
          Ok (Target.RecordConstructor fields)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Arbitrary ty ->
      let* domain = record_domain ~env ~source:ty in
      Ok (Target.Arbitrary domain)
  | Source.E_Var name -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Record _ -> Ok (Target.Reference name)
      | _ -> Error (Unsupported_expression source))
  | Source.E_Call call -> (
      let* ty = expression_type ~env source in
      match ty.Source.desc with
      | Source.T_Record _ -> function_call ~env ~source call
      | _ -> Error (Unsupported_expression source))
  | Source.E_Cond (condition, then_branch, else_branch) ->
      let* condition = boolean_expression ~env condition in
      let* then_branch = record_expression ~env then_branch in
      let* else_branch = record_expression ~env else_branch in
      Ok (Target.ConditionalExpression { condition; then_branch; else_branch })
  | Source.E_GetArray (base, index) ->
      let* base = array_expression ~env ~element_expression:record_expression ~element_domain:record_domain base in
      let* index = integer_expression ~env index in
      Ok (Target.ArrayAccess (base, index))
  | Source.E_GetItem (base, index) ->
      let* (TupleSelection (components, position)) =
        tuple_selection ~env ~source base index ~select:(fun ty ->
            match ty.Source.desc with
            | Source.T_Record _ -> Ok (record_expression, record_domain)
            | _ -> Error (Unsupported_expression source))
      in
      let* base = tuple_expression ~env ~components base in
      Ok (Target.TupleAccess (base, position))
  | Source.E_GetField (base, name) ->
      let* base = record_expression ~env base in
      Ok (Target.RecordAccess (base, name))
  | _ -> Error (Unsupported_expression source)

and expressions : type shape.
    env:Asllib.StaticEnv.env ->
    on_mismatch:error ->
    shape component_normalisers ->
    Source.expr list ->
    (shape Target.expressions, error) result =
 fun ~env ~on_mismatch components operands ->
  match (components, operands) with
  | NilNormaliser, [] -> Ok Target.Nil
  | ConsNormaliser (convert, _, rest), operand :: operands ->
      let* element = convert ~env operand in
      let* elements = expressions ~env ~on_mismatch rest operands in
      Ok (Target.Cons (element, elements))
  | _ -> Error on_mismatch

and function_call : type kind.
    env:Asllib.StaticEnv.env ->
    source:Source.expr ->
    Source.call ->
    (kind Target.expression, error) result =
 fun ~env ~source call ->
  match call.Source.call_type with
  | Source.ST_Function ->
      let* parameters = call_parameters ~env call.Source.params in
      let* (Expressions arguments) = call_arguments ~env call in
      Ok (Target.FunctionCall { name = call.Source.name; parameters = parameters; arguments = arguments })
  | Source.ST_Procedure | Source.ST_Getter | Source.ST_Setter -> Error (Unsupported_expression source)

and call_parameters ~env sources = map_result (integer_expression ~env) sources

and call_arguments ~env (call : Source.call) =
  match Asllib.ASTUtils.IMap.find_opt call.Source.name env.Asllib.StaticEnv.global.Asllib.StaticEnv.subprograms with
  | None -> Error (Unsupported_call call)
  | Some (signature, _) -> (
      match signature.Source.body with
      | Source.SB_Primitive _ -> Error (Unsupported_call call)
      | Source.SB_ASL _ ->
          let* (ComponentNormalisers components) =
            discover_components ~env (List.map snd signature.Source.args)
            |> Result.map_error (fun _ -> Unsupported_call call)
          in
          let* arguments = expressions ~env ~on_mismatch:(Unsupported_call call) components call.Source.args in
          Ok (Expressions arguments))

and integer_domain ~env ~source =
  match (Asllib.Types.make_anonymous env source).Source.desc with
  | Source.T_Int Source.UnConstrained -> Ok (Target.IntegerDomain None)
  | Source.T_Int (Source.WellConstrained (constraints, _)) -> integer_domain_constraints ~env ~source ~constraints
  | _ -> Error (Unsupported_domain source)

and integer_domain_constraints ~env ~source ~constraints =
  match constraints with
  | first :: constraints ->
      let* first = integer_constraint ~env first in
      let* constraints = map_result (integer_constraint ~env) constraints in
      Ok (Target.IntegerDomain (Some (first, constraints)))
  | [] -> Error (Unsupported_domain source)

and integer_constraint ~env source =
  match source with
  | Source.Constraint_Exact value ->
      let* value = integer_expression ~env value in
      Ok (value, value)
  | Source.Constraint_Range (lower, upper) ->
      let* lower = integer_expression ~env lower in
      let* upper = integer_expression ~env upper in
      Ok (lower, upper)

and boolean_domain ~env ~source =
  match (Asllib.Types.make_anonymous env source).Source.desc with
  | Source.T_Bool -> Ok Target.BooleanDomain
  | _ -> Error (Unsupported_domain source)

and bitvector_domain ~env ~source =
  match (Asllib.Types.make_anonymous env source).Source.desc with
  | Source.T_Bits (width, _) ->
      (* Typing resolves bitfield accesses to slices. ATCs and ARBITRARY need only the width. *)
      bitvector_domain_width ~env ~width
  | _ -> Error (Unsupported_domain source)

and bitvector_domain_width ~env ~width =
  let* width = integer_expression ~env width in
  Ok (Target.BitvectorDomain width)

and array_domain : type element.
    env:Asllib.StaticEnv.env ->
    element_domain:element domain_normaliser ->
    source:Source.ty ->
    (element Target.array_kind Target.domain, error) result =
 fun ~env ~element_domain ~source ->
  match (Asllib.Types.make_anonymous env source).Source.desc with
  | Source.T_Array (length, element_type) ->
      let* length = integer_expression ~env length in
      let* element_domain = element_domain ~env ~source:element_type in
      Ok (Target.ArrayDomain (length, element_domain))
  | _ -> Error (Unsupported_domain source)

and tuple_domain : type shape.
    env:Asllib.StaticEnv.env ->
    components:shape component_normalisers ->
    source:Source.ty ->
    (shape Target.tuple_kind Target.domain, error) result =
 fun ~env ~components ~source ->
  match (Asllib.Types.make_anonymous env source).Source.desc with
  | Source.T_Tuple sources -> (
      let* domains = component_domains ~env ~source components sources in
      match domains with
      | (Target.ConsDomain (_, Target.ConsDomain (_, _)) as domains) -> Ok (Target.TupleDomain domains : shape Target.tuple_kind Target.domain)
      | _ -> Error (Unsupported_domain source))
  | _ -> Error (Unsupported_domain source)

and record_domain ~env ~source =
  match (Asllib.Types.make_anonymous env source).Source.desc with
  | Source.T_Record fields -> record_domain_fields ~env ~fields
  | _ -> Error (Unsupported_domain source)

and record_domain_fields ~env ~fields =
  let field_domain (name, ty) =
    let* (AnyDomain domain) =
      domain_only ~env ~source:ty
      |> Result.map_error (fun error ->
          match error with
          | Unsupported_type ty -> Unsupported_domain ty
          | Expression_error error -> error)
    in
    Ok (Target.RecordFieldDomain (name, domain))
  in
  let* fields = map_result field_domain fields in
  Ok (Target.RecordDomain fields)

and component_domains : type shape.
    env:Asllib.StaticEnv.env ->
    source:Source.ty ->
    shape component_normalisers ->
    Source.ty list ->
    (shape Target.domains, error) result =
 fun ~env ~source components sources ->
  match (components, sources) with
  | NilNormaliser, [] -> Ok Target.NilDomain
  | ConsNormaliser (_, convert, rest), ty :: types ->
      let* domain = convert ~env ~source:ty in
      let* domains = component_domains ~env ~source rest types in
      Ok (Target.ConsDomain (domain, domains))
  | _ -> Error (Unsupported_domain source)

and normaliser_for_type ~env ty =
  let anonymous_ty = Asllib.Types.make_anonymous env ty in
  normaliser_for_type_desc ~env ~source:ty anonymous_ty.Source.desc

and normaliser_for_type_desc ~env ~source desc =
  match desc with
  | Source.T_Int _ -> Ok (Normaliser (integer_expression, integer_domain))
  | Source.T_Bool -> Ok (Normaliser (boolean_expression, boolean_domain))
  | Source.T_Bits _ -> Ok (Normaliser (bitvector_expression, bitvector_domain))
  | Source.T_Array (_, element_type) ->
      let* (Normaliser (element_expression, element_domain)) = normaliser_for_type ~env element_type in
      Ok (Normaliser (array_expression ~element_expression ~element_domain, array_domain ~element_domain))
  | Source.T_Tuple component_types ->
      let* (ComponentNormalisers components) = discover_components ~env component_types in
      Ok (Normaliser (tuple_expression ~components, tuple_domain ~components))
  | Source.T_Record _ -> Ok (Normaliser (record_expression, record_domain))
  | _ -> Error source

and discover_components ~env = function
  | [] -> Ok (ComponentNormalisers NilNormaliser)
  | ty :: rest ->
      let* (Normaliser (head, head_domain)) = normaliser_for_type ~env ty in
      let* (ComponentNormalisers tail) = discover_components ~env rest in
      Ok (ComponentNormalisers (ConsNormaliser (head, head_domain, tail)))

and tuple_selection : type kind.
    env:Asllib.StaticEnv.env ->
    source:Source.expr ->
    select:(Source.ty -> (kind normaliser * kind domain_normaliser, error) result) ->
    Source.expr ->
    int ->
    (kind tuple_selection, error) result =
 fun ~env ~source ~select base index ->
  let* ty = expression_type ~env base in
  match ty.Source.desc with
  | Source.T_Tuple types -> tuple_selection_from_types ~env ~source ~select types index
  | _ -> Error (Unsupported_expression source)

and tuple_selection_from_types : type kind.
    env:Asllib.StaticEnv.env ->
    source:Source.expr ->
    select:(Source.ty -> (kind normaliser * kind domain_normaliser, error) result) ->
    Source.ty list ->
    int ->
    (kind tuple_selection, error) result =
 fun ~env ~source ~select types index ->
  match (index, types) with
  | 0, ty :: rest ->
      let ty = Asllib.Types.make_anonymous env ty in
      let* (convert, convert_domain) = select ty in
      let* (ComponentNormalisers tail) =
        discover_components ~env rest |> Result.map_error (fun _ -> Unsupported_expression source)
      in
      Ok (TupleSelection (ConsNormaliser (convert, convert_domain, tail), Target.Here))
  | index, ty :: rest when index > 0 ->
      let* (Normaliser (head, head_domain)) = normaliser_for_type ~env ty |> Result.map_error (fun _ -> Unsupported_expression source) in
      let* (TupleSelection (tail, position)) = tuple_selection_from_types ~env ~source ~select rest (index - 1) in
      Ok (TupleSelection (ConsNormaliser (head, head_domain, tail), Target.There position))
  | _ -> Error (Unsupported_expression source)

and domain_only ~env ~source : (any_domain, domain_error) result =
  match (Asllib.Types.make_anonymous env source).Source.desc with
  | Source.T_Int Source.UnConstrained -> Ok (AnyDomain (Target.IntegerDomain None))
  | Source.T_Int (Source.WellConstrained (constraints, _)) ->
      let* domain = integer_domain_constraints ~env ~source ~constraints |> domain_expression_result in
      Ok (AnyDomain domain)
  | Source.T_Bool -> Ok (AnyDomain Target.BooleanDomain)
  | Source.T_Bits (width, _) ->
      let* domain = bitvector_domain_width ~env ~width |> domain_expression_result in
      Ok (AnyDomain domain)
  | Source.T_Array (length, element_type) ->
      let* length = integer_expression ~env length |> domain_expression_result in
      let* (AnyDomain element_domain) = domain_only ~env ~source:element_type in
      Ok (AnyDomain (Target.ArrayDomain (length, element_domain)))
  | Source.T_Tuple sources -> (
      let* (AnyDomains domains) = domains_only ~env ~sources in
      match domains with
      | (Target.ConsDomain (_, Target.ConsDomain (_, _)) as domains) -> Ok (AnyDomain (Target.TupleDomain domains))
      | _ -> Error (Unsupported_type source))
  | Source.T_Record fields ->
      let* domain = record_domain_fields ~env ~fields |> domain_expression_result in
      Ok (AnyDomain domain)
  | _ -> Error (Unsupported_type source)

and domains_only ~env ~sources : (any_domains, domain_error) result =
  match sources with
  | [] -> Ok (AnyDomains Target.NilDomain)
  | source :: rest ->
      let* (AnyDomain domain) = domain_only ~env ~source in
      let* (AnyDomains domains) = domains_only ~env ~sources:rest in
      Ok (AnyDomains (Target.ConsDomain (domain, domains)))

and domain ~env ~source =
  let anonymous_ty = Asllib.Types.make_anonymous env source in
  match anonymous_ty.Source.desc with
  | Source.T_Int Source.UnConstrained -> Ok (Domain (Target.IntegerDomain None, integer_expression, integer_domain))
  | Source.T_Int (Source.WellConstrained (constraints, _)) ->
      let* domain = integer_domain_constraints ~env ~source ~constraints |> domain_expression_result in
      Ok (Domain (domain, integer_expression, integer_domain))
  | Source.T_Bool -> Ok (Domain (Target.BooleanDomain, boolean_expression, boolean_domain))
  | Source.T_Bits (width, _) ->
      let* domain = bitvector_domain_width ~env ~width |> domain_expression_result in
      Ok (Domain (domain, bitvector_expression, bitvector_domain))
  | Source.T_Array (length, element_type) ->
      let* length = integer_expression ~env length |> domain_expression_result in
      let* (Domain (element_domain, element_expression, element_domain_normaliser)) = domain ~env ~source:element_type in
      Ok
        (Domain
           ( Target.ArrayDomain (length, element_domain),
             array_expression ~element_expression ~element_domain:element_domain_normaliser,
             array_domain ~element_domain:element_domain_normaliser ))
  | Source.T_Tuple sources -> (
      let* (Domains (domains, components)) = domains ~env ~sources in
      match domains with
      | (Target.ConsDomain (_, Target.ConsDomain (_, _)) as domains) ->
          Ok (Domain (Target.TupleDomain domains, tuple_expression ~components, tuple_domain ~components))
      | _ -> Error (Unsupported_type source))
  | Source.T_Record fields ->
      let* domain = record_domain_fields ~env ~fields |> domain_expression_result in
      Ok (Domain (domain, record_expression, record_domain))
  | _ -> Error (Unsupported_type source)

and domains ~env ~sources =
  match sources with
  | [] -> Ok (Domains (Target.NilDomain, NilNormaliser))
  | source :: rest ->
      let* (Domain (head_domain, head_expression, head_domain_normaliser)) = domain ~env ~source in
      let* (Domains (rest_domains, rest_components)) = domains ~env ~sources:rest in
      Ok
        (Domains
           (Target.ConsDomain (head_domain, rest_domains), ConsNormaliser (head_expression, head_domain_normaliser, rest_components)))
