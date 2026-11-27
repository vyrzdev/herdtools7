[@@@warnerror "-42"]

module Source = Asllib.AST
module Normalise = Typed_ast.Normalisation.Declaration
module Target = Typed_ast.Representation.AST

type error = {
  declaration : Source.decl;
  cause : Normalise.error;
}

let pp_declaration formatter (decl : Source.decl) =
  match decl.Source.desc with
  | Source.D_Func { Source.name; _ } -> Format.fprintf formatter "subprogram %S" name
  | Source.D_GlobalStorage { Source.name; _ } -> Format.fprintf formatter "global %S" name
  | Source.D_TypeDecl (name, _) -> Format.fprintf formatter "type %S" name
  | Source.D_Pragma (name, _) -> Format.fprintf formatter "pragma %S" name

let pp_error formatter { declaration; cause } =
  Format.fprintf formatter "@[<v>";
  if not (Asllib.ASTUtils.is_dummy_pos declaration) then
    Format.fprintf formatter "%a:@," Asllib.PP.pp_pos declaration;
  Format.fprintf formatter "TypedAST normalisation failed in %a:@,%a@]"
    pp_declaration declaration Normalise.pp_error cause

let (let*) = Result.bind

(* Typing must have succeeded before we get here.
   AST.t does not distinguish parsed input from typed input. *)
let rec run (ast : Asllib.AST.t) : (Target.declaration list, error) result =
  match ast with
  | [] -> Ok []
  | decl :: rest ->
      let* decl =
        Normalise.normalise_declaration decl
        |> Result.map_error (fun cause -> { declaration = decl; cause })
      in
      let* rest = run rest in
      Ok (decl :: rest)
