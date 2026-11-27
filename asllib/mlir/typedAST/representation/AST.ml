type integer = Integer_kind

type _ kind =
  | Integer : integer kind

(* TODO: Constructor names overlap; some uses need explicit types
   to disambiguate them. *)

(* Terms retain their kind but carry no further typing annotations.
   Giving a reference N its full singleton judgement integer {N} would
   introduce another N requiring the same judgement. *)
type 'kind term =
  | IntegerLiteral : Z.t -> integer term
  | IntegerAdd : integer term * integer term -> integer term
  (* Symbolic normalisation can turn n+n into 2*n. *)
  | IntegerMul : integer term * integer term -> integer term
  (* Carry a kind value so consumers can match on it when the index is abstract.
     The Reference constructor alone does not determine the kind. *)
  | Reference : 'kind kind * string -> 'kind term

let kind_of_term : type k. k term -> k kind = function
  | IntegerLiteral _ -> Integer
  | IntegerAdd _ -> Integer
  | IntegerMul _ -> Integer
  | Reference (kind, _) -> kind

type 'kind typing_judgement =
   (* Each pair is an inclusive (lower, upper) range; the list is their union.
      For simplicity, a singleton x is normalised to (x, x). *)
 | IntegerConstrained : (integer term * integer term) list -> integer typing_judgement
 | IntegerUnconstrained : integer typing_judgement

let kind_of_typing_judgement : type k. k typing_judgement -> k kind = function
  | IntegerConstrained _ -> Integer
  | IntegerUnconstrained -> Integer

type 'kind annotated_expr =
  | IntegerLiteral : Z.t -> integer annotated_expr (* Literals need no separate typing judgement here - their types are intrinsic! *)
  | IntegerAdd : integer typing_judgement * integer annotated_expr * integer annotated_expr -> integer annotated_expr
  | Reference : 'kind typing_judgement * string -> 'kind annotated_expr
  | Arbitrary : 'kind source_type -> 'kind annotated_expr

(* Symbolic normalisation handles our literal/reference/ADD bounds. It copies
   the original type annotation onto the new root, leaving its operands unannotated.
   However, it would leave some expressions, e.g. n MOD 2, unchanged,
   including their operand annotations.
   Implement Annotated now for such bounds when we support them. *)
and 'kind source_bound =
  | Annotated of 'kind annotated_expr
  | RootAnnotated of 'kind typing_judgement * 'kind term

(* TODO: Working name! *)
and 'kind source_type =
   (* Each pair is an inclusive (lower, upper) range; the list is their union.
      For simplicity, a singleton x is normalised to (x, x). *)
 | IntegerConstrained : (integer source_bound * integer source_bound) list -> integer source_type
 | IntegerUnconstrained : integer source_type

let kind_of_source_type : type k. k source_type -> k kind = function
  | IntegerConstrained _ -> Integer
  | IntegerUnconstrained -> Integer

let kind_of_expression : type k. k annotated_expr -> k kind = function
  | IntegerLiteral _ -> Integer
  | IntegerAdd _ -> Integer
  | Reference (judgement, _) -> kind_of_typing_judgement judgement
  | Arbitrary ty -> kind_of_source_type ty

type procedure_return = Procedure_return

(* The statement index describes the enclosing subprogram's return kind.
   Sharing the index with the function's return type checks the kind of
   returned values, not whether they satisfy its constraints or whether
   a return occurs on every path. *)
type 'kind statement =
  | Pass : 'kind statement
  | Sequence : 'kind statement * 'kind statement -> 'kind statement
  (* The declaration's value kind is existential, independent of the return kind.
     Its optional source type and initialiser share that kind. *)
  | ImmutableDeclaration : {
      name : string;
      source_type : 'value_kind source_type option;
      initialiser : 'value_kind annotated_expr;
    } -> 'kind statement
  | MutableDeclaration : {
      name : string;
      source_type : 'value_kind source_type option;
      initialiser : 'value_kind annotated_expr;
    } -> 'kind statement
  | Return : 'kind annotated_expr -> 'kind statement
  | ProcedureReturn : procedure_return statement

type declaration =
  | Function : {
      name : string;
      return_type : 'kind source_type;
      body : 'kind statement;
    } -> declaration
  | Procedure : {
      name : string;
      body : procedure_return statement;
    } -> declaration
