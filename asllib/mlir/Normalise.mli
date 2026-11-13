module Expression = NormaliseExpression
module Statement = NormaliseStatement

type some_arguments = Arguments : 'shape IR.AST.arguments -> some_arguments [@@unboxed]

type some_domain = Expression.some_domain =
  | Domain : 'kind IR.AST.domain * 'kind Expression.normaliser * 'kind Expression.domain_normaliser -> some_domain

type error =
  | Unsupported_pragma
  | Unsupported_getter
  | Unsupported_setter
  | Unsupported_primitive
  | Unsupported_config
  | Expression_error of Expression.error
  | Statement_error of Statement.error
  | Missing_parameter_type_annotation of Asllib.AST.identifier
  | Unsupported_parameter_type of Asllib.AST.identifier * Asllib.AST.ty
  | Unsupported_argument_type of Asllib.AST.identifier * Asllib.AST.ty
  | Missing_return_type of Asllib.AST.identifier
  | Unsupported_return_type of Asllib.AST.ty
  | Missing_global_initial_value of Asllib.AST.identifier
  | Unsupported_global_type of Asllib.AST.identifier * Asllib.AST.ty

val parameter :
  env:Asllib.StaticEnv.env -> name:Asllib.AST.identifier -> ty_opt:Asllib.AST.ty option -> (IR.AST.parameter, error) result

val arguments : env:Asllib.StaticEnv.env -> sources:Asllib.AST.typed_identifier list -> (some_arguments, error) result

val return_domain : env:Asllib.StaticEnv.env -> source:Asllib.AST.ty -> (some_domain, error) result

val recurse_limit :
  env:Asllib.StaticEnv.env -> source:Asllib.AST.expr option -> (IR.AST.integer IR.AST.expression option, error) result

val qualifier : source:Asllib.AST.func_qualifier option -> IR.AST.subprogram_qualifier option

val subprogram : env:Asllib.StaticEnv.global -> source:Asllib.AST.func -> (IR.AST.declaration, error) result

val global_storage : env:Asllib.StaticEnv.global -> source:Asllib.AST.global_decl -> (IR.AST.declaration, error) result

val declaration : env:Asllib.StaticEnv.global -> source:Asllib.AST.decl -> (IR.AST.declaration option, error) result

val run :
  env:Asllib.StaticEnv.global -> Asllib.AST.t -> (IR.AST.t, error) result
