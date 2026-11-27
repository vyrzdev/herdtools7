module Target = Representation.AST

(* Do not renumber or reuse violation references. *)
type error =
  | Violation of int (* An assumption of the normaliser has failed. *)
  | Unimplemented (* Outside the supported subset. *)
  | PrecisionLoss (* For now, we defer precision lost type annotations - requires further review. *)
  | ArbitraryInAnnotation (* For now, we defer handling of annotations containing arbitrary - requires further review. *)

type some_kind = Kind : 'kind Target.kind -> some_kind [@@unboxed]
