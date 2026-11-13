open Asllib
open AST
open ASTUtils
open! Testhelpers.Helpers
open Infix

let _dbg = false

let env_with_N =
  let open StaticEnv in
  add_local "N" integer LDK_Let empty

let build_consts () =
  let values =
    [ ("c1", !$3); ("c2", !!(E_Slice (!%"c1", [ Slice_Range (!$3, !$0) ]))) ]
  in
  let consts =
    List.map
      (fun (name, e) ->
        D_GlobalStorage
          { name; keyword = GDK_Let; ty = None; initial_value = Some e }
        |> __POS_OF__ |> add_pos_from_pos_of)
      values
  in
  let main =
    D_Func
      {
        name = "main";
        body = SB_ASL !!(S_Return (Some !$0));
        args = [];
        recurse_limit = None;
        parameters = [];
        return_type = Some integer;
        subprogram_type = ST_Function;
        qualifier = None;
        override = None;
        builtin = false;
      }
    |> __POS_OF__ |> add_pos_from_pos_of
  in
  let ast = main :: consts in
  let _ = Typing.type_and_run ast in
  ()

let normalize () =
  let do_one (e1, e2, env) =
    let e1' = StaticModel.normalize env e1 in
    let e2' = StaticModel.normalize env e2 in
    let () =
      if _dbg then Format.eprintf "%a ---> %a@." PP.pp_expr e1 PP.pp_expr e1'
    in
    let () =
      if _dbg then Format.eprintf "%a ---> %a@." PP.pp_expr e2 PP.pp_expr e2'
    in
    assert (StaticModel.equal_in_env env e1 e2)
  in

  List.iter do_one
    [
      (binop `SUB !$4 !$2, !$2, StaticEnv.empty);
      ( binop `ADD (binop `SUB !%"N" !%"m") (binop `SUB !%"m" !$1),
        binop `SUB !%"N" !$1,
        StaticEnv.add_local "m" integer LDK_Let env_with_N );
      (neg !$3, !$(-3), StaticEnv.empty);
    ]

let constraint_bounds () =
  let ast =
    Builder.from_string ~filename:"constraint-bounds.asl"
      ~ast_string:
        {|
config a: integer{1..3} = 1;
config b: integer{1..3} = 1;
func bounds{N: integer{
  a+1, (a+b) DIV 2 .. (a DIV 2)*2,
  if '1111'[0+:(a+1)] == '1111'[a:0] then 1 else 2
}}(x: bits(N))
begin pass; end;
|}
      `ASLv1
  in
  let bounds ast =
    List.find_map
      (fun declaration ->
        match declaration.desc with
        | D_Func
            {
              name = "bounds";
              parameters =
                [
                  ( _,
                    Some { desc = T_Int (WellConstrained (constraints, _)); _ }
                  );
                ];
              _;
            } ->
            Some constraints
        | _ -> None)
      ast
    |> Option.get
  in
  let typed, _ = Typing.TypeCheckDefault.type_check_ast ast in
  let constraints = bounds typed in
  let a = !%"a" and b = !%"b" in
  let bits = literal (L_BitVector (Asllib.Bitvector.ones 4)) in
  let slice length = !!(E_Slice (bits, [ Slice_Length (!$0, length) ])) in
  let expected =
    [
      Constraint_Exact (binop `ADD a !$1);
      Constraint_Range
        (binop `DIV (binop `ADD a b) !$2, binop `MUL (binop `DIV a !$2) !$2);
      Constraint_Exact
        !!(E_Cond
             ( binop `EQ
                 (slice (binop `ADD a !$1))
                 (slice (binop `ADD (binop `SUB a !$0) !$1)),
               !$1,
               !$2 ));
    ]
  in
  assert (List.equal (constraint_equal (fun _ _ -> false)) expected constraints);
  let rec check_annotations e =
    assert (Option.is_some e.ty_opt);
    match e.desc with
    | E_Binop (_, left, right) ->
        check_annotations left;
        check_annotations right
    | E_Cond (condition, yes, no) ->
        check_annotations condition;
        check_annotations yes;
        check_annotations no
    | E_Slice (base, slices) ->
        check_annotations base;
        List.iter
          (fun slice ->
            match slice with
            | Slice_Length (offset, length) ->
                check_annotations offset;
                check_annotations length
            | _ -> assert false)
          slices
    | E_Var _ | E_Literal _ -> ()
    | _ -> assert false
  in
  List.iter
    (fun constraint_ ->
      match constraint_ with
      | Constraint_Exact value -> check_annotations value
      | Constraint_Range (lower, upper) ->
          check_annotations lower;
          check_annotations upper)
    constraints

let constraint_division () =
  let run source =
    Builder.from_string ~filename:"constraint-division.asl" ~ast_string:source
      `ASLv1
    |> Typing.type_and_run |> fst
  in
  assert (
    run
      {|
config a: integer{1, 3} = 1;
config b: integer{1, 3} = 1;
func main() => integer begin
  let exact = ARBITRARY: integer{(a+b) DIV 2};
  let range = ARBITRARY: integer{(a+b) DIV 2 .. (a+b) DIV 2};
  let checked = 1 as integer{(a+b) DIV 2};
  let sliced = '1111'[0+:((a+b) DIV 2)];
  assert exact == 1;
  assert range == 1;
  assert checked == 1;
  assert sliced == '1111'[0+:((a+b) DIV 2)];
  return 0;
end;
|}
    = 0);
  List.iter
    (fun declaration ->
      let source =
        Printf.sprintf
          "config a: integer{1..2} = 1;\n\
           func main() => integer begin\n\
           %s\n\
           return 0;\n\
           end;\n"
          declaration
      in
      let failed =
        try
          let _ = run source in
          false
        with
        | Error.ASLException
            {
              desc =
                Error.UnsupportedBinop (Error.Dynamic, `DIV, L_Int a, L_Int b);
              _;
            }
        ->
          Z.equal a Z.one && Z.equal b (Z.of_int 2)
      in
      assert failed)
    [
      "let value = ARBITRARY: integer{(a DIV 2)*2};";
      "let value = ARBITRARY: integer{(a DIV 2)*2 .. 4};";
      "let value = 1 as integer{(a DIV 2)*2};";
      "let value = '1111'[0+:((a DIV 2)*2)];";
      "let value = '1111'[(a DIV 2)*2:0];";
    ]

let () =
  exec_tests
    [
      ("build_consts", build_consts);
      ("static.normalize", normalize);
      ("typing.constraint_bounds", constraint_bounds);
      ("typing.constraint_division", constraint_division);
    ]
