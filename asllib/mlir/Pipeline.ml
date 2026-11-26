type error = Unimplemented

(* Typing must have succeeded before we get here.
   AST.t does not distinguish parsed input from typed input. *)
let run (_ : Asllib.AST.t) : (unit, error) result =
  Error Unimplemented
