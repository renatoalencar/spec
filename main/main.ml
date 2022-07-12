
let code =
  {|
   (module
    (func $add (param i32) (param i32) (result i32)
      (local i32)
      local.tee 2
      local.get 0
      i32.add)
    (func $sub (param i32) (param i32) (result i32)
      local.get 1
      local.get 0
      i32.sub)
    (func $mul (param i64) (param i64) (result i64)
      local.get 1
      local.get 0
      i64.mul)
    (func $divs (param i64) (param i64) (result i64)
      local.get 1
      local.get 0
      i64.div_s)
    (func $example1 (param i32) (result i32)
      i32.const 42
      local.get 0
      i32.add))
  |}

let definition = 
  match (Parse.string_to_module code).it with
  | Script.Textual m -> m
  | _ -> assert false

open Types
open Ast
open Values

let wasm_type_to_llvm context typ =
  match typ with
  | NumType I32Type -> Llvm.i32_type context
  | NumType I64Type -> Llvm.i64_type context
  | NumType F32Type -> Llvm.float_type context
  | NumType F64Type -> Llvm.double_type context
  | _ -> assert false

let wasm_func_type_to_llvm context typ =
  match typ with
  | FuncType (params, [ return ]) ->
    Llvm.function_type
      (wasm_type_to_llvm context return)
      (Array.of_list (List.map (wasm_type_to_llvm context) params))
  | _ -> assert false

let emit_func idx modul typ func =
  let context = Llvm.module_context modul in
  let fn =
    let fn_name = Printf.sprintf "func%d" idx in
    let fn_type = wasm_func_type_to_llvm context typ in
    Llvm.define_function fn_name fn_type modul
  in
  let locals =
    let locals =
      func.Source.it.locals
      |> List.map (fun ty ->
          Llvm.const_int (wasm_type_to_llvm context ty) 0)
      |> Array.of_list
    in
    Array.append (Llvm.params fn) locals
  in
  let builder =
    let entry = Llvm.entry_block fn in
    Llvm.builder_at_end context entry
  in

  let int_bin_op op =
    match op with
    | IntOp.Add -> Llvm.build_add
    | Sub -> Llvm.build_sub
    | Mul -> Llvm.build_mul
    | DivS -> Llvm.build_sdiv
    | DivU -> Llvm.build_udiv
    | _ -> assert false
  in

  let bin_op op a b =
    match op with
    | I32 op | I64 op -> int_bin_op op a b "tmp" builder
    | _ -> assert false
  in

  let const num =
    match num with
    | I32 v -> Llvm.(const_int (i32_type context) (Int32.to_int v))
    | I64 v -> Llvm.(const_of_int64 (i64_type context) v true)
    | _ -> assert false
  in

  let step stack instr =
    match instr.Source.it, stack with
    | Nop, _ -> stack
    | Drop, _ :: stack -> stack
    | Return, top :: [] -> Llvm.build_ret top builder :: []
    | Const num, stack -> const num.it :: stack
    | LocalGet { it ; _ }, stack -> locals.(Int32.to_int it) :: stack
    | LocalSet { it ; _ }, top :: stack ->
        locals.(Int32.to_int it) <- top;
        stack
    | LocalTee { it ; _ }, top :: _ ->
        locals.(Int32.to_int it) <- top;
        stack
    | Binary op, a :: b :: stack -> bin_op op a b :: stack
    | _ -> assert false
  in
  match List.fold_left step [] func.Source.it.body; with
  | [] -> ignore (Llvm.build_ret_void builder)
  | top :: [] -> ignore (Llvm.build_ret top builder)
  | _ -> failwith "Stack should return only one item"

let () =
  let context = Llvm.create_context () in
  let modul = Llvm.create_module context "module" in

  let ast =
    let Source.{ it = definition ; _ } = Parse.string_to_module code in
    match definition with
    | Textual ast -> ast
    | _ -> assert false
  in
  let () =
    List.iteri
      (fun idx fn ->
        let fn_type = List.nth ast.it.types (Int32.to_int fn.Source.it.ftype.it) in
        emit_func idx modul fn_type.Source.it fn)
      ast.it.funcs
  in
  Llvm.dump_module modul;
  assert (Llvm_executionengine.initialize ());
  let engine = Llvm_executionengine.create modul in
  let ty = Foreign.funptr Ctypes.(int @-> int @-> returning int) in
  let fn = Llvm_executionengine.get_function_address "func0" ty engine in
  Printf.printf "Function call result: %d\n" (fn 4 4);
  Llvm.dispose_module modul;
  Llvm.dispose_context context
