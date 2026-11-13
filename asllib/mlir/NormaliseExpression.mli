type error =
  | Missing_type_annotation of Asllib.AST.expr
  | Unsupported_expression of Asllib.AST.expr
  | Unsupported_call of Asllib.AST.call
  | Unsupported_slice of Asllib.AST.slice
  | Unsupported_domain of Asllib.AST.ty

type 'kind normaliser =
  env:Asllib.StaticEnv.env -> Asllib.AST.expr -> ('kind IR.AST.expression, error) result

type 'kind domain_normaliser =
  env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> ('kind IR.AST.domain, error) result

type _ component_normalisers =
  | NilNormaliser : unit component_normalisers
  | ConsNormaliser : 'kind normaliser * 'kind domain_normaliser * 'rest component_normalisers -> ('kind * 'rest) component_normalisers

type some_normaliser = Normaliser : 'kind normaliser * 'kind domain_normaliser -> some_normaliser

type some_domain = Domain : 'kind IR.AST.domain * 'kind normaliser * 'kind domain_normaliser -> some_domain

type domain_error =
  | Unsupported_type of Asllib.AST.ty
  | Expression_error of error

type some_component_normalisers = ComponentNormalisers : 'shape component_normalisers -> some_component_normalisers [@@unboxed]

type some_expressions = Expressions : 'shape IR.AST.expressions -> some_expressions [@@unboxed]

val call_parameters :
  env:Asllib.StaticEnv.env -> Asllib.AST.expr list -> (IR.AST.integer IR.AST.expression list, error) result

val call_arguments : env:Asllib.StaticEnv.env -> Asllib.AST.call -> (some_expressions, error) result

val normaliser_for_type : env:Asllib.StaticEnv.env -> Asllib.AST.ty -> (some_normaliser, Asllib.AST.ty) result
val normaliser_for_type_desc : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> Asllib.AST.type_desc -> (some_normaliser, Asllib.AST.ty) result

val domain : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> (some_domain, domain_error) result

val discover_components : env:Asllib.StaticEnv.env -> Asllib.AST.ty list -> (some_component_normalisers, Asllib.AST.ty) result

val annotated_type :
  env:Asllib.StaticEnv.env ->
  ('desc, Asllib.AST.ty) Asllib.AST.t_annotated ->
  (Asllib.AST.ty, ('desc, Asllib.AST.ty) Asllib.AST.t_annotated) result

val expression_type : env:Asllib.StaticEnv.env -> Asllib.AST.expr -> (Asllib.AST.ty, error) result
val integer_expression : env:Asllib.StaticEnv.env -> Asllib.AST.expr -> (IR.AST.integer IR.AST.expression, error) result
val boolean_expression : env:Asllib.StaticEnv.env -> Asllib.AST.expr -> (IR.AST.boolean IR.AST.expression, error) result

val bitvector_expression :
  env:Asllib.StaticEnv.env -> Asllib.AST.expr -> (IR.AST.bitvector IR.AST.expression, error) result

val record_expression : env:Asllib.StaticEnv.env -> Asllib.AST.expr -> (IR.AST.record_kind IR.AST.expression, error) result

val integer_domain : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> (IR.AST.integer IR.AST.domain, error) result
val integer_domain_constraints : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> constraints:Asllib.AST.int_constraint list -> (IR.AST.integer IR.AST.domain, error) result
val boolean_domain : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> (IR.AST.boolean IR.AST.domain, error) result
val bitvector_domain : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> (IR.AST.bitvector IR.AST.domain, error) result
val record_domain : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> (IR.AST.record_kind IR.AST.domain, error) result

val array_domain :
  env:Asllib.StaticEnv.env ->
  element_domain:'element domain_normaliser ->
  source:Asllib.AST.ty ->
  ('element IR.AST.array_kind IR.AST.domain, error) result

val tuple_domain :
  env:Asllib.StaticEnv.env ->
  components:'shape component_normalisers ->
  source:Asllib.AST.ty ->
  ('shape IR.AST.tuple_kind IR.AST.domain, error) result

val slice : env:Asllib.StaticEnv.env -> Asllib.AST.slice -> (IR.AST.slice, error) result
val slices : env:Asllib.StaticEnv.env -> Asllib.AST.slice list -> (IR.AST.slice list, error) result

val array_expression :
  env:Asllib.StaticEnv.env ->
  element_expression:'element normaliser ->
  element_domain:'element domain_normaliser ->
  Asllib.AST.expr ->
  ('element IR.AST.array_kind IR.AST.expression, error) result

val tuple_expression :
  env:Asllib.StaticEnv.env ->
  components:'shape component_normalisers ->
  Asllib.AST.expr ->
  ('shape IR.AST.tuple_kind IR.AST.expression, error) result
