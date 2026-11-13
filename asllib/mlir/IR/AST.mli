type integer = Integer_kind
type boolean = Boolean_kind
type bitvector = Bitvector_kind
type 'element array_kind = Array_kind
type 'shape tuple_kind = Tuple_kind
type record_kind = Record_kind

type (_, _) tuple_position =
  | Here : ('kind * 'rest, 'kind) tuple_position
  | There : ('rest, 'kind) tuple_position -> ('head * 'rest, 'kind) tuple_position

type _ expression =
  (* Literals and references *)
  | IntegerLiteral : Z.t -> integer expression
  | BooleanLiteral : bool -> boolean expression
  | BitvectorLiteral : Asllib.Bitvector.t -> bitvector expression
  | Reference : string -> 'kind expression
  (* Domain-bearing expressions *)
  | Arbitrary : 'kind domain -> 'kind expression
  | IntegerAssertedConversion : integer expression * integer domain -> integer expression
  | BitvectorAssertedConversion : bitvector expression * bitvector domain -> bitvector expression
  | TupleAssertedConversion : 'shape tuple_kind expression * 'shape tuple_kind domain -> 'shape tuple_kind expression
  (* Integer operations *)
  | IntegerNegate : integer expression -> integer expression
  | IntegerAdd : integer expression * integer expression -> integer expression
  | IntegerSubtract : integer expression * integer expression -> integer expression
  | IntegerMultiply : integer expression * integer expression -> integer expression
  | IntegerExactDivide : integer expression * integer expression -> integer expression
  | IntegerFloorDivide : integer expression * integer expression -> integer expression
  | IntegerRemainder : integer expression * integer expression -> integer expression
  | IntegerPower : integer expression * integer expression -> integer expression
  | IntegerShiftLeft : integer expression * integer expression -> integer expression
  | IntegerShiftRight : integer expression * integer expression -> integer expression
  (* Integer comparisons *)
  | IntegerEqual : integer expression * integer expression -> boolean expression
  | IntegerNotEqual : integer expression * integer expression -> boolean expression
  | IntegerLessThan : integer expression * integer expression -> boolean expression
  | IntegerLessOrEqual : integer expression * integer expression -> boolean expression
  | IntegerGreaterThan : integer expression * integer expression -> boolean expression
  | IntegerGreaterOrEqual : integer expression * integer expression -> boolean expression
  (* Boolean operations *)
  | BooleanNot : boolean expression -> boolean expression
  | BooleanAnd : boolean expression * boolean expression -> boolean expression
  | BooleanOr : boolean expression * boolean expression -> boolean expression
  | BooleanImplies : boolean expression * boolean expression -> boolean expression
  | BooleanEquivalent : boolean expression * boolean expression -> boolean expression
  | BooleanEqual : boolean expression * boolean expression -> boolean expression
  | BooleanNotEqual : boolean expression * boolean expression -> boolean expression
  (* Bitvector operations *)
  | BitvectorNot : bitvector expression -> bitvector expression
  | BitvectorAnd : bitvector expression * bitvector expression -> bitvector expression
  | BitvectorOr : bitvector expression * bitvector expression -> bitvector expression
  | BitvectorXor : bitvector expression * bitvector expression -> bitvector expression
  | BitvectorConcat : bitvector expression * bitvector expression -> bitvector expression
  | BitvectorAdd : bitvector expression * bitvector expression -> bitvector expression
  | BitvectorSubtract : bitvector expression * bitvector expression -> bitvector expression
  | BitvectorIntegerAdd : bitvector expression * integer expression -> bitvector expression
  | BitvectorIntegerSubtract : bitvector expression * integer expression -> bitvector expression
  (* Bitvector comparisons *)
  | BitvectorEqual : bitvector expression * bitvector expression -> boolean expression
  | BitvectorNotEqual : bitvector expression * bitvector expression -> boolean expression
  (* Slicing *)
  | IntegerSlice : integer expression * slice * slice list -> bitvector expression
  | BitvectorSlice : bitvector expression * slice * slice list -> bitvector expression
  (* Arrays *)
  | ArrayCreate : { length : integer expression; value : 'element expression } -> 'element array_kind expression
  | ArrayAccess : 'element array_kind expression * integer expression -> 'element expression
  (* Tuples *)
  | TupleConstructor : ('first * ('second * 'rest)) expressions -> ('first * ('second * 'rest)) tuple_kind expression
  | TupleAccess : 'shape tuple_kind expression * ('shape, 'kind) tuple_position -> 'kind expression
  (* Records *)
  | RecordConstructor : record_field list -> record_kind expression
  | RecordAccess : record_kind expression * string -> 'kind expression
  | RecordFieldsAccess : record_kind expression * string * string list -> bitvector expression
  (* Calls *)
  | FunctionCall : {
      name : string;
      parameters : integer expression list;
      arguments : 'shape expressions;
    } -> 'kind expression
  (* Conditionals *)
  | ConditionalExpression : {
      condition : boolean expression;
      then_branch : 'kind expression;
      else_branch : 'kind expression;
    } -> 'kind expression

and _ expressions =
  | Nil : unit expressions
  | Cons : 'kind expression * 'rest expressions -> ('kind * 'rest) expressions

and record_field = RecordField : string * 'kind expression -> record_field

and slice = { offset : integer expression; length : integer expression }
and integer_constraint = integer expression * integer expression

(* A domain describes permitted values. Its enclosing construct decides
   whether its expressions are evaluated; bindings introduce no evaluation. *)
and _ domain =
  | IntegerDomain : (integer_constraint * integer_constraint list) option -> integer domain
  | BooleanDomain : boolean domain
  | BitvectorDomain : integer expression -> bitvector domain
  | ArrayDomain : integer expression * 'element domain -> 'element array_kind domain
  | TupleDomain : ('first * ('second * 'rest)) domains -> ('first * ('second * 'rest)) tuple_kind domain
  | RecordDomain : record_field_domain list -> record_kind domain

and _ domains =
  | NilDomain : unit domains
  | ConsDomain : 'kind domain * 'rest domains -> ('kind * 'rest) domains

and record_field_domain = RecordFieldDomain : string * 'kind domain -> record_field_domain

type parameter = { name : string; domain : integer domain }

type _ arguments =
  | NilArgument : unit arguments
  | ConsArgument : string * 'kind domain * 'rest arguments -> ('kind * 'rest) arguments

type whole = Whole
type component = Component
type allow_discard = Allow_discard
type no_discard = No_discard

type record_field_slice = { field : string; offset : int; length : int }

type (_, _, _) target =
  | DiscardTarget : ('kind, 'position, allow_discard) target
  | VariableTarget : string -> ('kind, 'position, 'discard) target
  | ArrayElementTarget : ('element array_kind, component, no_discard) target * integer expression -> ('element, 'position, 'discard) target
  | BitvectorSliceTarget : (bitvector, component, no_discard) target * slice * slice list -> (bitvector, 'position, 'discard) target
  | RecordFieldTarget : (record_kind, component, no_discard) target * string -> ('kind, 'position, 'discard) target
  | RecordFieldsTarget : string * record_field_slice * record_field_slice * record_field_slice list -> (bitvector, whole, 'discard) target
  | TupleTarget : ('first * ('second * 'rest)) targets -> (('first * ('second * 'rest)) tuple_kind, whole, 'discard) target

and _ targets =
  | NilTarget : unit targets
  | ConsTarget : ('kind, component, allow_discard) target * 'rest targets -> ('kind * 'rest) targets

type _ binding =
  | VariableBinding : string -> 'kind binding
  | TupleBinding : ('first * ('second * 'rest)) bindings -> ('first * ('second * 'rest)) tuple_kind binding

and _ bindings =
  | NilBinding : unit bindings
  | ConsBinding : string * 'rest bindings -> ('kind * 'rest) bindings

type for_direction = Up | Down

type procedure_return = Procedure_return
type 'kind function_return = Function_return

type _ statement =
  (* Structure *)
  | Pass : 'return_kind statement
  | Sequence : 'return_kind statement * 'return_kind statement -> 'return_kind statement
  (* Local declarations *)
  | MutableDeclaration : { binding : 'kind binding; domain : 'kind domain option; initial_value : 'kind expression } -> 'return_kind statement
  | ImmutableDeclaration : { binding : 'kind binding; domain : 'kind domain option; initial_value : 'kind expression } -> 'return_kind statement
  (* Assignment and discard *)
  | Assignment : ('kind, whole, allow_discard) target * 'kind expression -> 'return_kind statement
  (* Calls *)
  | ProcedureCall : {
      name : string;
      parameters : integer expression list;
      arguments : 'shape expressions;
    } -> 'return_kind statement
  (* Conditionals *)
  | Conditional : { condition : boolean expression; then_branch : 'return_kind statement; else_branch : 'return_kind statement } -> 'return_kind statement
  (* Loops *)
  | While : {
      condition : boolean expression;
      body : 'return_kind statement;
      limit : integer expression option;
    } -> 'return_kind statement
  | Repeat : {
      body : 'return_kind statement;
      until_condition : boolean expression;
      limit : integer expression option;
    } -> 'return_kind statement
  | For : {
      index_name : string;
      start : integer expression;
      direction : for_direction;
      end_ : integer expression;
      body : 'return_kind statement;
      limit : integer expression option;
    } -> 'return_kind statement
  (* Runtime checks *)
  | Assert : boolean expression -> 'return_kind statement
  | Unreachable : 'return_kind statement
  (* Returns *)
  | Return : 'kind expression -> 'kind function_return statement
  | ProcedureReturn : procedure_return statement

type subprogram_qualifier = Pure | Readonly | Noreturn

type declaration =
  | GlobalConstant : { name : string; domain : 'kind domain option; initial_value : 'kind expression } -> declaration
  | GlobalLet : { name : string; domain : 'kind domain option; initial_value : 'kind expression } -> declaration
  | GlobalVariable : { name : string; domain : 'kind domain option; initial_value : 'kind expression } -> declaration
  | Function : {
      name : string;
      parameters : parameter list;
      arguments : 'shape arguments;
      return_domain : 'kind domain;
      recurse_limit : integer expression option;
      qualifier : subprogram_qualifier option;
      body : 'kind function_return statement;
    } -> declaration
  | Procedure : {
      name : string;
      parameters : parameter list;
      arguments : 'shape arguments;
      recurse_limit : integer expression option;
      qualifier : subprogram_qualifier option;
      body : procedure_return statement;
    } -> declaration

type t = declaration list
