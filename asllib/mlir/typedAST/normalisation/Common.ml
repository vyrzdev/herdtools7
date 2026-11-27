module Target = Representation.AST

(* Do not renumber or reuse violation references. *)
type error =
  | Violation of int (* An assumption of the normaliser has failed. *)
  | Unimplemented (* Outside the supported subset. *)
  | PrecisionLoss (* For now, we defer precision lost type annotations - requires further review. *)
  | ArbitraryInAnnotation (* For now, we defer handling of annotations containing arbitrary - requires further review. *)

let pp_error formatter = function
  | Violation reference ->
      Format.fprintf formatter "Violation #%d: an assumption of the normaliser failed" reference
  | Unimplemented ->
      Format.pp_print_string formatter "Unimplemented: outside the supported subset"
  | PrecisionLoss ->
      Format.pp_print_string formatter "PrecisionLoss: types with lost precision are not supported"
  | ArbitraryInAnnotation ->
      Format.pp_print_string formatter "ArbitraryInAnnotation: ARBITRARY in a typing annotation is not supported"

type some_kind = Kind : 'kind Target.kind -> some_kind [@@unboxed]
