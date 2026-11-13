type error =
  | Expression_error of NormaliseExpression.error
  | Missing_target_type_annotation of Asllib.AST.lexpr
  | Unsupported_target of Asllib.AST.lexpr
  | Invalid_return of Asllib.AST.stmt
  | Unsupported_statement of Asllib.AST.stmt

type _ return_kind =
  | FunctionReturn : 'kind NormaliseExpression.normaliser -> 'kind IR.AST.function_return return_kind
  | ProcedureReturn : IR.AST.procedure_return return_kind

val statement :
  env:Asllib.StaticEnv.env ->
  return_kind:'return_kind return_kind ->
  Asllib.AST.stmt ->
  ('return_kind IR.AST.statement, error) result

