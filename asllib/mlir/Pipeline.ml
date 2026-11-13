let run (ast : Asllib.AST.t) =
  let ast, env = Asllib.Typing.TypeCheckDefault.type_check_ast ast in
  Normalise.run ~env ast
