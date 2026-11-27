module Target = Representation.AST
module Bindings = Asllib.ASTUtils.IMap

(* Term references need the binding's kind because they have no annotation.
   Annotated references use the annotation attached to the occurrence. *)
type binding = Binding : 'kind Target.kind -> binding
type t = binding Bindings.t

let empty : t = Bindings.empty
let add name binding (env : t) = Bindings.add name binding env
let find_opt name (env : t) = Bindings.find_opt name env
