(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module External = struct
  type during_data = { mutable asm_code : X86_ast.asm_line list }

  type 'state data =
    | Unavailable : [< `Before_emit | `Waiting_to_be_assembled] data
    | Available : during_data -> [`During_emit] data

  type target =
    | File of
        { name : string;
          keep_asm : bool;
          channel : Out_channel.t
        }
    | Stdout

  let channel = function File { channel;name; _ } ->
    print_endline name; channel | Stdout -> stdout

  let cleanup = function
    | File { channel; name; keep_asm } ->
      if not keep_asm then Misc.remove_file name;
      close_out channel
    | Stdout -> ()


  let begin_emit t Unavailable =
    Emitaux.output_channel := channel t;
    let data = { asm_code = [] } in
    data, Available data

  (* val end_emit : during_emit -> waiting_to_be_assembled

  val assemble : obj_filename:string -> waiting_to_be_assembled -> int *)
end

type internal_section = X86_section_name.Tbl.key * X86_ast.asm_line list

type internal_emitter =
  delayed:(unit -> internal_section list) ->
  internal_section list ->
  string ->
  unit

module Internal = struct
  type during_data =
    { f : internal_emitter;
      asm_code_current_section : X86_ast.asm_line list ref ref;
      asm_code_by_section : X86_ast.asm_line list ref X86_section_name.Tbl.t;
      delayed_sections : X86_ast.asm_line list ref X86_section_name.Tbl.t
    }

  let create_data f =
    { f;
      asm_code_current_section = ref (ref []);
      asm_code_by_section = X86_section_name.Tbl.create 100;
      delayed_sections = X86_section_name.Tbl.create 100
    }

  type 'state data =
    | Registered : internal_emitter -> [`Before_emit] data
    | Pending : during_data -> [< `During_emit] data
    | Done : (string -> unit) -> [`Waiting_to_be_assembled] data

  type target = unit
end

type global_data =
  | Gerror_if_accessed
  | Gdo_not_emit
  | Gexternal of External.during_data ref
  | Ginternal of Internal.during_data ref

let global_data = ref Gerror_if_accessed

type 'state t =
  | Do_not_emit : 'state t
  | Internal : Internal.target * 'state Internal.data -> 'state t
  | External : External.target * 'state External.data -> 'state t

type before_emit = [`Before_emit] t

type during_emit = [`During_emit] t

type waiting_to_be_assembled = [`Waiting_to_be_assembled] t

let do_not_emit = Do_not_emit

let to_stdout () = External (Stdout, Unavailable)

let using_external_assembler ~keep_asm ~asm_filename =
  External
    ( File { name = asm_filename; keep_asm; channel = open_out asm_filename },
      Unavailable )

let using_internal_assembler f = Internal ((), Registered f)

let begin_emit (t : before_emit) =
  assert (match !global_data with Gerror_if_accessed -> true | _ -> false);
  match t with
  | Do_not_emit ->
    global_data := Gdo_not_emit;
    Do_not_emit
  | External (target, state) ->
    let data, next = External.begin_emit target state in
    global_data := Gexternal (ref data);
    External (target, next)
  | Internal (common, Registered f) ->
    let data = Internal.create_data f in
    global_data := Ginternal (ref data);
    Internal (common, Pending data)

let end_emit_assert_consistency_global_data t =
  let c =
    match t, !global_data with
    | External _, Gexternal _ -> true
    | Internal _, Ginternal _ -> true
    | Do_not_emit, Gdo_not_emit -> true
    | _ -> false
  in
  assert c

let x86_generate_code = function
  | External (target, Available { asm_code }) ->
    Emitaux.Dwarf_helpers.emit_dwarf ();
    let asm = List.rev asm_code in
    let generate =
      if X86_proc.masm then X86_masm.generate_asm else X86_gas.generate_asm
    in
    let channel = External.channel target in
    Profile.record ~accumulate:true "write_asm" (generate channel) asm;
    close_out channel;
    Emitaux.output_channel := stderr;
    (External (target, Unavailable) : waiting_to_be_assembled)
  | Internal ((), Pending { f; asm_code_by_section; delayed_sections; _ }) ->
    let get sections =
      X86_section_name.Tbl.fold
        (fun name instrs acc -> (name, List.rev !instrs) :: acc)
        sections []
    in
    let instrs = get asm_code_by_section in
    let delayed () = get delayed_sections in
    (Internal ((), Done (f ~delayed instrs)) : waiting_to_be_assembled)
  | Do_not_emit -> Do_not_emit

let end_emit (t : during_emit) =
  let next = x86_generate_code t in
  global_data := Gerror_if_accessed;
  next

let assemble ~obj_filename = function
  | External (File { name; _ }, _) -> X86_proc.compile name obj_filename
  | External (Stdout, _) -> assert false
  | Internal ((), Done f) ->
    f obj_filename;
    0
  | Do_not_emit -> 0

let cleanup = function
  | External (target, _) -> External.cleanup target
  | Internal _ -> ()
  | Do_not_emit -> ()

module For_x86 = struct
  let directive dir =
    match !global_data with
    | Gdo_not_emit -> ()
    | Gexternal d ->
      let d = !d in
      d.asm_code <- dir :: d.asm_code
    | Ginternal d -> (
      let d = !d in
      match dir with
      | X86_ast.Section (name, flags, args, is_delayed) -> (
        let name = X86_section_name.make name flags args in
        let where =
          if is_delayed then d.delayed_sections else d.asm_code_by_section
        in
        match X86_section_name.Tbl.find_opt where name with
        | Some x -> d.asm_code_current_section := x
        | None ->
          d.asm_code_current_section := ref [];
          X86_section_name.Tbl.add where name !(d.asm_code_current_section))
      | dir ->
        !(d.asm_code_current_section) := dir :: !(!(d.asm_code_current_section)))
    | Gerror_if_accessed -> assert false

  let emit ins = directive (Ins ins)
end
