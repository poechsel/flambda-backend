(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* From lambda to assembly code *)

(* CR poechsel: Remove -33-60-27-70-32 *)
[@@@ocaml.warning "+a-4-9-40-41-42-33-60-27-70-32"]

open Format
open Config
open Clflags
open Misc
open Cmm

module DLL = Flambda_backend_utils.Doubly_linked_list

module String = Misc.Stdlib.String

module Reg = struct
  module T = struct
    include Reg
    let compare (a:Reg.t) (b:Reg.t) =
      match a.loc, b.loc with
      | Unknown, Unknown -> Int.compare a.stamp b.stamp
      | Reg x, Reg y -> 
        let c = Int.compare x y in
        if c = 0 then Int.compare a.stamp b.stamp
        else c
      | Stack _, _ -> assert false
      | _, Stack _ -> assert false
      | Unknown, Reg _ -> -1
      | Reg _, Unknown -> -1
  end

  include T
end
(* 
(** Returns a set of registers written over by the basic block *)
let basic_block_write_over_regs (t : Cfg.basic_block) : Reg.Set.t =
  let of_instruction s (instr : _ Cfg.instruction) =
    Array.fold_left (fun x s -> Reg.Set.add s x) s instr.res
  in
  let init = of_instruction Reg.Set.empty t.terminator in
  DLL.fold_left t.body ~f:of_instruction ~init *)

let get_register_read_in_bb (t : Cfg.basic_block) : Reg.Set.t =
  let of_instruction s (instr : _ Cfg.instruction) =
    Array.fold_left (fun x s -> Reg.Set.add s x) s instr.arg
  in
  let init = of_instruction Reg.Set.empty t.terminator in
  DLL.fold_left t.body ~f:of_instruction ~init

let get_all_registers_in_bb (t : Cfg.basic_block) : Reg.Set.t =
  let of_instruction s (instr : _ Cfg.instruction) =
    let s = 
      Array.fold_left (fun x s -> Reg.Set.add s x) s instr.arg
    in
    Array.fold_left (fun x s -> Reg.Set.add s x) s instr.res
  in
  let init = of_instruction Reg.Set.empty t.terminator in
  DLL.fold_left t.body ~f:of_instruction ~init

module Llir = struct
  module Type = struct
    let of_memory_chunk = function
      | Byte_unsigned | Byte_signed -> "i8", None
      | Sixteen_unsigned | Sixteen_signed -> "i16", None
      | Thirtytwo_unsigned | Thirtytwo_signed -> "i32", None
      | Word_int | Word_val -> "ptr", None
      | Single -> "float", None
      | Double -> "double", Some 8 (* word-aligned 64-bit float *)
      | Onetwentyeight_unaligned -> "i128", None
      | Onetwentyeight_aligned -> "i128", Some 16 (* 16-byte-aligned 128-bit vector *)
  
    let of_ocaml = function
      | Val -> "ptr"
      | Addr -> "ptr"
      | Int -> "i64"
      | Float -> "double"
      | Vec128 -> "i128"

    let align = function
      (* CR poechsel: find a better value here*)
      | _ -> 8
  end

  let current_lbl = ref None

  module Var = struct
    type t =
      | Arg of Reg.t
      | Reg of { stamp : int; lbl: Label.t option; r: Reg.t }
      | Cond of (string * int)
      | Statepoint of int

    let print_lbl ppf = function
      | None -> ()
      | Some l -> Format.fprintf ppf "%d_" l
    
    let print_reg ppf (reg : Reg.t) =
      Format.fprintf ppf "%d" reg.stamp

    let print ppf = function
      | Arg r ->
        Format.fprintf ppf "%%arg_%a" print_reg r
      | Cond (prefix, r) ->
        Format.fprintf ppf "%%%s_%d" prefix r
      | Reg { stamp; lbl; r } ->
        Format.fprintf ppf "%%reg_%d_%a%a" stamp print_lbl lbl print_reg r
      | Statepoint stamp -> Format.fprintf ppf "%%statepoint_%d" stamp

    let stamp = ref 0

    let buildin prefix = incr stamp; Cond (prefix, !stamp)

    let statepoint () = incr stamp; Statepoint !stamp

    let of_reg r = incr stamp; Reg { stamp = !stamp; lbl = !current_lbl; r }

    let compare = 
      (* CR poechsel: Hand write this comparison operator *)
      compare


    module Map = Map.Make(struct
      type nonrec t = t
      let compare = compare
    end)
  end

  type t = {
    regs : Var.t Reg.Map.t;
    globals : Reg.Set.t
  }
  let emit_global_reg ppf (reg : Reg.t) =
    Format.fprintf ppf "%%glb_%d" reg.stamp

  let empty ~globals ~args =
    let regs = 
      args
      |> Array.to_seq
      |> Seq.map (fun r -> r, Var.Arg r)
      |> Reg.Map.of_seq
    in
    { regs; globals }

  let with_context ~args ~lbl f =
    current_lbl := Some lbl;
    let t = args in
    let x = f t in
    current_lbl := None;
    x

  let get reg t =
    match Reg.Map.find_opt reg t.regs with
    | Some v -> v
    | None -> assert false

  type dst_value =
      | Reg of Reg.t
      | Value of Var.t

  let is_global reg t =
    Reg.Set.mem t reg.globals
        
  let emit_store_global ppf ~(src:Reg.t) ~(dst:Reg.t) t =
    let typ = src.typ in
    let src = get src t in
    Format.fprintf ppf "  store %s %a, ptr %a\n"
      (Type.of_ocaml typ)
      Var.print src
      emit_global_reg dst;
    (* poechsel: To avoid reinserting [loads] when not needed we 
        store a binding of the value stored *)
    { t with regs = Reg.Map.add dst src t.regs }

  let emit'' ?(value:dst_value option) ppf_dump (t : t) =
    Format.fprintf ppf_dump "  ";
    let store_to, t = match value with
      | None -> None, t
      | Some (Value v) -> 
        Format.fprintf ppf_dump  "%a = " Var.print v;
        None, t
      | Some (Reg reg) -> 
        let v : Var.t = Var.of_reg reg in
        let t = { t with regs = Reg.Map.add reg v t.regs } in
        Format.fprintf ppf_dump  "%a = " Var.print v;
        if is_global t reg then Some reg, t
        else None, t
    in
    Format.kfprintf (fun ppf ->
      Format.fprintf ppf "\n"; 
      match store_to with
      | None -> t
      | Some reg -> emit_store_global ppf_dump ~src:reg ~dst:reg t
    ) 
    ppf_dump

  let emit' ~value ppf_dump t = emit'' ~value:(Value value) ppf_dump t

  let emit ?value ppf_dump t = emit'' ?value:(Option.map (fun r -> Reg r) value) ppf_dump t

  let debug t =
    Format.fprintf Format.std_formatter "DEBUG\n"; 
    Reg.Map.iter (fun k v ->
      Format.fprintf Format.std_formatter "   %d -> %a\n" k.stamp Var.print v
      ) t.regs;
    Format.fprintf Format.std_formatter "\n\n"


  let alias ~src ~dst t =
    let v = match Reg.Map.find_opt src t.regs with
      | None -> assert false
      | Some v -> v
    in
    debug t;
    { t with regs = Reg.Map.add dst v t.regs }

  let emit_alloca_for_globals ppf globals =
    Reg.Set.iter (fun reg ->
        Format.fprintf ppf "  %a = alloca %s, align %d\n"
          emit_global_reg reg
          (Type.of_ocaml reg.typ)
          (Type.align reg.typ)
      )
      globals

  let emit_load_for_globals ~bb ppf t =
    let bb_globals = get_register_read_in_bb bb in
    let bb_globals =
      Reg.Set.filter (fun reg ->
        Reg.Set.mem reg t.globals && not (Reg.Map.mem reg t.regs))
        bb_globals 
    in
    let regs =
      Reg.Set.fold (fun reg regs ->
          let var = Var.of_reg reg in
          (* CR poechsel: check alignement *)
          Format.fprintf ppf "  %a = load %s, ptr %a\n" 
            Var.print var 
            (Type.of_ocaml reg.typ)
            emit_global_reg reg;
          Reg.Map.add reg var t.regs 
        ) 
        bb_globals  
        t.regs
    in
    { t with regs }
end

module RegCountMap = struct
  type t = int Reg.Map.t

  let of_array l = Reg.Map.of_seq (Seq.map (fun v -> v, 1) (Array.to_seq l))
  let add (t:t) reg : t =
    match Reg.Map.find_opt reg t with
    | None -> Reg.Map.add reg 1 t
    | Some v -> Reg.Map.add reg (v+1) t
  let to_set t = Reg.Map.to_seq t |> Seq.map fst |> Reg.Set.of_seq
end

let register_being_written_once_in_bb (t : Cfg.basic_block) =
  let of_instruction regs (instr : _ Cfg.instruction) =
    Array.fold_left RegCountMap.add regs instr.res
  in
  let init = Reg.Map.empty in
  let reg_counts = DLL.fold_left t.body ~f:of_instruction ~init in
  let reg_counts = of_instruction reg_counts t.terminator in
  Reg.Map.filter (fun _ v -> v <= 1) reg_counts
  |> RegCountMap.to_set

(** Potential alloca are all registers that are not local.
    A register is considered as local if it only happens within a block and
    is only assigned once. *)
let get_potential_alloca ~args (cfg : Cfg.t) : Reg.Set.t =
  let reg_counts = 
    let init = RegCountMap.of_array args in
    Cfg.fold_blocks cfg ~init ~f:(fun _lbl bb acc ->
      Reg.Set.fold (fun r a -> RegCountMap.add a r)
        (get_all_registers_in_bb bb) 
        acc
    )
  in
  let all_regs = RegCountMap.to_set reg_counts in
  let all_regs_in_only_one_block = 
    Reg.Map.filter (fun _ v -> v = 1) reg_counts
    |> RegCountMap.to_set
  in
  let known_locals = 
    Cfg.fold_blocks cfg ~init:Reg.Set.empty ~f:(fun _lbl bb acc ->
      (Reg.Set.inter
      (register_being_written_once_in_bb bb)
      all_regs_in_only_one_block
      )
      |> Reg.Set.union acc
    )
    in
    Reg.Set.diff all_regs known_locals

(** Builds a set of successors for every basic block *)
let build_successors_table cfg =
  let successors = Label.Tbl.create 10 in
  Cfg.iter_blocks cfg ~f:(fun lbl _bb ->
    Label.Tbl.add successors lbl Label.Set.empty);
  Cfg.iter_blocks cfg ~f:(fun lbl bb ->
    Label.Set.iter (fun pred ->
        let s = Label.Tbl.find successors pred in
        Label.Tbl.replace successors pred (Label.Set.add lbl s)
      ) bb.predecessors 
  );
  successors

let print_label ppf lbl = Format.fprintf ppf "%%l%d" lbl
let print_label_def ppf lbl = Format.fprintf ppf "l%d" lbl

let emit_int_test ~llir_context ~ppf_dump ~args { Cfg.lt; eq; gt; is_signed; imm } =
  let lbl_true = eq in
  let lbl_false = if lt = lbl_true then gt else lt in
  (* Reverse operation than the one done in cfgize *)
  let value = Llir.Var.buildin "cond" in
  let emit_rhs ppf () = 
    match imm with
    | Some imm -> Format.fprintf ppf "%d" imm
    | None -> Llir.Var.print ppf (Llir.get args.(1) llir_context)
  in
  let emit which =
    let llir_context =
      Llir.emit' ~value ppf_dump llir_context "icmp %s %s %a, %a"
        which
        (Llir.Type.of_ocaml args.(0).Reg.typ)
        Llir.Var.print (Llir.get args.(0) llir_context)
        emit_rhs ()
    in
    Llir.emit ppf_dump llir_context "br i1 %a, label %a, label %a"
      Llir.Var.print value
      print_label lbl_true
      print_label lbl_false
  in
  match (lt = lbl_true), (eq = lbl_true), (gt = lbl_true) with
  | false, true, false -> (* Eq *) emit "eq"
  | false, true, true -> (* Ge *)
    if is_signed then
      emit "sge"
    else
      emit "ge"
  | true, true, false -> (* Le *)
    if is_signed then
      emit "sle"
    else
      emit "le"
  | _, false, _ -> (* Dead branch by construction *) assert false
  | _ -> assert false

let emit_terminator ~llir_context ~ppf_dump ~args ~res (terminator : Cfg.terminator) =
  let unhandled d =
    Array.iter (fun x ->Format.fprintf ppf_dump "ARG: %s %d: \n" (Reg.name x) x.Reg.stamp ) args;
    Array.iter (fun x ->Format.fprintf ppf_dump "RET: %s %d: \n" (Reg.name x) x.Reg.stamp ) res;
    Format.fprintf ppf_dump "terminator unhandled %s" d;
    if true then assert false;
    llir_context
  in
  let llir_context =
    begin match terminator with
    | Never -> unhandled "Never\n"
    | Always lbl -> Llir.emit ppf_dump llir_context "br label %a" print_label lbl
    | Parity_test b -> unhandled "Parity_test\n"
    | Truth_test b -> unhandled "Truth_test\n"
    | Float_test f -> unhandled "Float_test\n"
    | Int_test i ->
      assert (Array.length res = 0);
      emit_int_test ~llir_context ~ppf_dump ~args i
    | Switch arms -> unhandled "Switch\n"
    | Return -> 
      if Array.length args = 0 then
        Llir.emit ppf_dump llir_context "ret void"
      else if Array.length args = 1 then
        Llir.emit ppf_dump llir_context "ret %s %a"
          (Llir.Type.of_ocaml args.(0).Reg.typ)
          Llir.Var.print (Llir.get args.(0) llir_context )
      else assert false
    | Raise r -> unhandled "Raise\n"
    | Tailcall_self { destination = _} -> unhandled "Tailcall_self\n"
    | Tailcall_func f -> unhandled "Tailcall_func\n"
    | Call_no_return e -> unhandled "Call_no_return\n"
    | Call f -> unhandled "Call\n"
    | Prim { op = Probe _; label_after }  ->
      unhandled "Prim probe\n"
    | Prim { op = External { func_symbol; ty_res; ty_args; alloc; stack_ofs = _ }; label_after }  ->
      assert (not Config.runtime5);
      (* CR poechsel: Do something with the result *)
      let ret_type res =
        match res with
        | [||] -> "void"
        | [| t |] -> Llir.Type.of_ocaml t
        | _ -> assert false
      in
      let ext_type = function
        | XInt -> "i64"
        | XInt32 -> "i32"
        | XInt64 -> "i64"
        | XFloat -> "double"
        | XVec128 -> "i128"
      in
      if alloc then begin
        let intrinsics_overloaded ~args ~res =
          let disc = ret_type res in
          List.fold_left (fun acc arg ->
            Format.sprintf "%s.%s" acc (ext_type arg)
            ) disc args
        in

        (* CR poechsel: Calling convention? *)
        Format.fprintf Format.err_formatter "SAFEPOINT IMLEMNT plz\n";
        let id_statepoint = 0 (* CR poechsel: Use something else *) in
        let num_patch_bytes = 0 in
        let emit_arg_types ppf_dump () =
          for i = 0 to Array.length args - 1 do
            if i > 0 then
              Format.fprintf ppf_dump ", ";
            Format.fprintf ppf_dump "%s" (ext_type (List.nth ty_args i))
          done
        in
        let emit_arg_and_types ppf_dump () =
          for i = 0 to Array.length args - 1 do
            if i > 0 then
              Format.fprintf ppf_dump ", ";
            Format.fprintf ppf_dump "%s %a"
              (ext_type (List.nth ty_args i))
              Llir.Var.print (Llir.get args.(i) llir_context)
          done
        in
        let disc = intrinsics_overloaded ~args:ty_args ~res:ty_res in
        let value = Llir.Var.statepoint () in
        let llir_context : Llir.t =
          Llir.emit' ~value ppf_dump llir_context 
            "call token @llvm.experimental.gc.statepoint.%s (i64 %d, i32 %d, %s (%a)* %s, i64 %d, i64 0, %a, i64 0, i64 0)"
            disc
            id_statepoint
            num_patch_bytes
            (ret_type ty_res)
            emit_arg_types ()
            func_symbol
            (Array.length args)
            emit_arg_and_types ()
        in
        let llir_context =
          match ty_res with
          | [| |] -> (* void, no need for a gc result *) llir_context
          | _ ->
            Llir.emit ~value:res.(0) ppf_dump llir_context
              "call %s @llvm.experiment.result(token %a)" (ret_type ty_res) Llir.Var.print value
        in
        Llir.emit ppf_dump llir_context "br label %a" print_label label_after
      end else
        (* CR poechsel: Calling convention? *)
        let emit_arg_types ppf_dump () =
          for i = 0 to Array.length args - 1 do
            if i > 0 then
              Format.fprintf ppf_dump ", ";
            Format.fprintf ppf_dump "%s %a"
              (ext_type (List.nth ty_args i))
              Llir.Var.print (Llir.get args.(i) llir_context)
          done
        in
        let llir_context : Llir.t =
          Llir.emit ~value:res.(0) ppf_dump llir_context "call %s @%s(%a)"
            (ret_type ty_res)
            func_symbol
            emit_arg_types ()
        in
        Llir.emit ppf_dump llir_context "br label %a" print_label label_after
    | Specific_can_raise  a -> unhandled "Specific_can_raise\n"
    end
  in
  llir_context, None, None

let llvm_intop (op : Mach.integer_operation) =
  match op with
  | Iadd -> "add"
  | Iasr -> "ashr"
  | Isub -> assert false
  | Imul -> assert false
  | Imulh _ -> assert false
  | Idiv -> "sdiv"
  | Imod -> assert false
  | Iand -> assert false
  | Ior -> assert false
  | Ixor -> assert false
  | Ilsl -> "shl"
  | Ilsr -> "lshr"
  | Iclz _ -> assert false
  | Ictz _ -> assert false
  | Ipopcnt -> assert false
  | Icomp comp ->
    begin match comp with
    | Isigned _ -> assert false
    | Iunsigned _ -> assert false
  end

let new_cst_value = 
  let counter = ref 0 in
  fun () ->
    incr counter;
    "cst_" ^ string_of_int !counter

let emit_instruction ~llir_context ~ppf_dump ~args ~res (instruction : Cfg.basic) =
  let unhandled d =
    Array.iter (fun x ->Format.fprintf ppf_dump "ARG: %s %d: \n" (Reg.name x) x.Reg.stamp ) args;
    Array.iter (fun x ->Format.fprintf ppf_dump "RET: %s %d: \n" (Reg.name x) x.Reg.stamp ) res;
    Format.fprintf ppf_dump "instr unhandled %s" d;
    assert false
  in
  match instruction with 
  | Op op -> 
    begin match op with
    | Move  ->
      let src : Reg.t = args.(0) in
      let dst : Reg.t = res.(0)  in
      if src.typ <> dst.typ then
        Llir.emit ~value:dst ppf_dump llir_context "bitcast %s %a to %s"
          (Llir.Type.of_ocaml dst.typ)
          Llir.Var.print (Llir.get src llir_context)
          (Llir.Type.of_ocaml src.typ) 
      else if Llir.is_global llir_context dst then Llir.emit_store_global ppf_dump ~dst ~src llir_context 
      else Llir.alias ~dst ~src llir_context
      (*if src.loc <> dst.loc then
        Llir.emit ~value:dst ppf_dump llir_context "bitcast %s %a to %s"
          (Llir.Type.of_ocaml dst.typ)
          Llir.Var.print (Llir.get src llir_context)
          (Llir.Type.of_ocaml src.typ)
      else llir_context *)
    | Spill  -> unhandled "Spill\n"
    | Reload  -> unhandled "Reload\n"
    | Const_int v -> 
      (* Const int are done by cheating. We insert a add [value] 0 to make sure llvm understand which type they are *) 
      Llir.emit ~value:(res.(0)) ppf_dump llir_context "add %s 0, %Ld" (Llir.Type.of_ocaml res.(0).Reg.typ) (Int64.of_nativeint v)
    | Const_float v -> unhandled "Const_float\n"
    | Const_symbol v -> 
      (* CR poechsel: FIX *)
      Llir.emit ~value:(res.(0)) ppf_dump llir_context "@%s" v.sym_name
    | Const_vec128  v -> unhandled "Const_vec128\n"
    | Stackoffset  v -> unhandled "Stackoffset\n"
    | Load  { memory_chunk; addressing_mode = Iindexed 0; mutability = _; is_atomic }  -> 
      let typ, align = Llir.Type.of_memory_chunk memory_chunk in
      let align = match align with | None -> "" | Some v -> sprintf ", align %d" v in
      (* CR poechsel: check if unordered is the good ordering *)
      let atomic, ordering = if is_atomic then " atomic", " unordered" else "", "" in
      (* Volatile because GC operations will be volatile too *)
      Llir.emit ~value:(res.(0)) ppf_dump llir_context 
        "load%s volatile %s, ptr %a %s %s" 
        atomic
        typ
        Llir.Var.print (Llir.get args.(0) llir_context)
        ordering
        align
    | Store (memory_chunk, Iindexed 0, assignment) -> 
      let typ, align = Llir.Type.of_memory_chunk memory_chunk in
      let align = match align with | None -> "" | Some v -> sprintf ", align %d" v in
      let atomic, ordering = 
        (* Cf OCaml5 spec - we need to generate a barrier sometimes *)
        (* CR poechsel: Is this actually true? *)
        match memory_chunk with
        | Word_int | Word_val when assignment -> " atomic", "seq_cst"
        | _ -> "", ""
      in
      Llir.emit ppf_dump llir_context
        "store%s volatile %s %a, ptr %a %s %s"
        atomic
        typ
        Llir.Var.print (Llir.get args.(0) llir_context)
        Llir.Var.print (Llir.get args.(1) llir_context)
        ordering
        align
    | Store _ ->
      unhandled "Store\n"
    | Load _ ->
      unhandled "Load\n"
    | Intop op ->
      Llir.emit ~value:(res.(0)) ppf_dump llir_context 
        "%s %s %a, %a" 
        (llvm_intop op) 
        (Llir.Type.of_ocaml res.(0).Reg.typ)
        Llir.Var.print (Llir.get args.(0) llir_context)
        Llir.Var.print (Llir.get args.(1) llir_context)
    
    | Intop_imm   (op, i) ->
      Llir.debug llir_context;
      Llir.emit ~value:(res.(0)) ppf_dump llir_context 
        "%s %s %a, %d" 
        (llvm_intop op)
        (Llir.Type.of_ocaml res.(0).Reg.typ)
         Llir.Var.print (Llir.get args.(0) llir_context)
        i
    | Intop_atomic  { op = _; size = _; addr = _ } -> unhandled "Intop_atomic\n"
    | Negf  -> unhandled "Negf\n"
    | Absf  -> unhandled "Absf\n"
    | Addf  -> unhandled "Addf\n"
    | Subf  -> unhandled "Subf\n"
    | Mulf  -> unhandled "Mulf\n"
    | Divf  -> unhandled "Divf\n"
    | Compf  cpf -> unhandled "Compf\n"
    | Csel  test -> unhandled "Csel\n"
    | Floatofint  -> unhandled "Floatofint\n"
    | Intoffloat  -> unhandled "Intoffloat\n"
    | Valueofint  -> unhandled "Valueofint\n"
    | Intofvalue  -> unhandled "Intofvalue\n"
    | Vectorcast  v -> unhandled "Vectorcast\n"
    | Scalarcast  s -> unhandled "Scalarcast\n"
    | Probe_is_enabled  { name = _ } -> unhandled "Probe_is_enabled\n"
    | Opaque  -> unhandled "Opaque\n"
    | Begin_region  -> unhandled "Begin_region\n"
    | End_region  -> unhandled "End_region\n"
    | Specific spec -> unhandled "Specific\n"
    | Name_for_debugger  _ -> unhandled "Name_for_debugger\n"
    | Dls_get  -> unhandled "Dls_get\n"
    | Poll  -> unhandled "Poll\n"
    | Alloc  { bytes = _; dbginfo = _; mode = _ } -> unhandled "Alloc\n"
    end
  | Reloadretaddr -> Format.fprintf Format.err_formatter "Unhandlned reloadretaddr\n"; (* CR poechsel *) llir_context
  | Pushtrap { lbl_handler = _ } -> unhandled "pushtrap\n"
  | Poptrap -> unhandled "poptrap\n"
  | Prologue -> (* CR poechsel *) llir_context


let emit_fundecl ~ppf_dump cfg_with_layout = 
  let { Cfg.blocks = _;
      fun_name;
      fun_args;
      fun_dbg = _;
      entry_label = _;
      fun_fast = _;
      fun_contains_calls = _;
      fun_num_stack_slots  = _
    } = Cfg_with_layout.cfg cfg_with_layout in
  print_endline fun_name;

  Cfg_with_layout.dump ppf_dump ~msg:"" cfg_with_layout;
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let layout = Cfg_with_layout.layout cfg_with_layout in
  let tailrec_label = ref None in
  let globals = get_potential_alloca ~args:fun_args cfg in
  let args = Llir.empty ~globals ~args:fun_args in

  (* Assumes all functions are returning a value. *)
  (* CR poechsel: get a better type here *)
  Format.fprintf ppf_dump "define ptr %s(" fun_name;
  
  Array.iteri (fun i arg ->
    if i > 0 then Format.fprintf ppf_dump ", ";
    Format.fprintf ppf_dump "%s %a"
      (Llir.Type.of_ocaml arg.Reg.typ) 
      Llir.Var.print (Llir.get arg args)
    ) fun_args;
  Format.fprintf ppf_dump ") {\n";

  Llir.emit_alloca_for_globals ppf_dump globals;
  
  (* Store args in the alloca zone if they are globals *)
  let args =
    Array.fold_left
      (fun context reg ->
        if Llir.is_global context reg then
          Llir.emit_store_global ppf_dump context ~src:reg ~dst:reg
        else context
    ) args
    fun_args
  in
  Format.fprintf ppf_dump "  br label %a\n" print_label cfg.entry_label;
  DLL.iter layout ~f:(fun label ->
      if not (Label.Tbl.mem cfg.blocks label)
      then Misc.fatal_errorf "Unknown block labelled %d\n" label;
      let block = Label.Tbl.find cfg.blocks label in
      assert (Label.equal label block.start);
      Format.fprintf ppf_dump "%a:\n" print_label_def label;
      Llir.with_context ~args ~lbl:label (fun llir_context ->
        let llir_context =
          Llir.emit_load_for_globals ppf_dump ~bb:block llir_context
        in
        let llir_context =
          DLL.fold_left
            ~init:llir_context
            ~f:(fun llir_context (i : _ Cfg.instruction) -> 
              (* Cfg.dump_basic ppf_dump i.desc; *)
              emit_instruction ~llir_context ~ppf_dump ~args:i.arg ~res:i.res i.desc) 
            block.body
        in
        let llir_context, terminator, terminator_tailrec_label = 
          emit_terminator ~llir_context ~ppf_dump ~args:block.terminator.arg ~res:block.terminator.res block.terminator.desc
        in
        (* CR poechsel: do something with it for phi nodes*)
        ignore llir_context;
        (match !tailrec_label, terminator_tailrec_label with
        | (Some _ | None), None -> ()
        | None, Some _ -> tailrec_label := terminator_tailrec_label
        | Some old_trl, Some new_trl -> assert (Label.equal old_trl new_trl)));
  );
  Format.fprintf ppf_dump "}\n\n";
  if false then
  assert false
  (*
  let fun_contains_calls = cfg.fun_contains_calls in
  let fun_num_stack_slots = cfg.fun_num_stack_slots in
  let fun_frame_required =
    Proc.frame_required ~fun_contains_calls ~fun_num_stack_slots
  in
  let fun_prologue_required =
    Proc.prologue_required ~fun_contains_calls ~fun_num_stack_slots
  in
  let fun_section_name =
    if !Flambda_backend_flags.basic_block_sections
    then CL.get_section cfg_with_layout cfg.entry_label
    else None
  in
  { Linear.fun_name = cfg.fun_name;
    fun_args = Reg.set_of_array cfg.fun_args;
    fun_body = !next.insn;
    fun_tailrec_entry_point_label = !tailrec_label;
    fun_fast = cfg.fun_fast;
    fun_dbg = cfg.fun_dbg;
    fun_contains_calls;
    fun_num_stack_slots;
    fun_frame_required;
    fun_prologue_required;
    fun_section_name
  }
 *)
  (* Array.iter (fun x -> 
    Format.fprintf ppf_dump "ARG: %s %d: \n" (Reg.name x) x.Reg.stamp ) fun_args;
  Cfg_with_layout.dump ppf_dump ~msg:"" cfg;
  let () = 
    Cfg_with_layout.fold_instructions
      ~instruction:(fun acc x -> emit_instruction ~ppf_dump ~args:x.arg ~res:x.res acc x.desc)
      ~terminator:(fun acc x -> emit_terminator ~ppf_dump ~args:x.arg ~res:x.res acc x.desc)
      ~init:()
      cfg
  in
  if true then assert false *)


(* The below should likely go in asmgen *)
let (++) x f = f x

let compile_data _ =
  print_endline "TODO emit_data"

let compile_fundecl ~ppf_dump ~funcnames cmms =
  Proc.init ();
  Reg.reset();
  cmms
  ++ Profile.record ~accumulate:true "selection"
       (Llvm_selection.fundecl ~future_funcnames:funcnames)
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_sel
  ++ Profile.record ~accumulate:true "polling"
       (Polling.instrument_fundecl ~future_funcnames:funcnames)
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_polling
  ++ Profile.record ~accumulate:true "checkmach"
       (Checkmach.fundecl ~future_funcnames:funcnames ppf_dump)
  (* ++ Profile.record ~accumulate:true "comballoc" Comballoc.fundecl
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_combine *)
  ++ Profile.record ~accumulate:true "cfgize" (
    Cfgize.fundecl
      ~before_register_allocation:true
      ~preserve_orig_labels:false
      ~simplify_terminators:true)
  (* ++ Cfg_with_infos.make *)
  ++ emit_fundecl ~ppf_dump

let compile_phrases ~ppf_dump ps =
  let funcnames =
    List.fold_left (fun s p ->
        match p with
        | Cfunction fd -> String.Set.add fd.fun_name.sym_name s
        | Cdata _ -> s)
      String.Set.empty ps
  in
  let rec compile ~funcnames ps =
    match ps with
    | [] -> ()
    | p :: ps ->
        if !dump_cmm then fprintf ppf_dump "%a@." Printcmm.phrase p;
        match p with
        | Cfunction fd ->
          compile_fundecl ~ppf_dump ~funcnames fd;
          compile ~funcnames:(String.Set.remove fd.fun_name.sym_name funcnames) ps
        | Cdata dl ->
          compile_data dl;
          compile ~funcnames ps
  in
  compile ~funcnames ps

let emit _unix ~sourcefile ~ppf_dump cmms =
  print_endline sourcefile;
  compile_phrases ~ppf_dump cmms