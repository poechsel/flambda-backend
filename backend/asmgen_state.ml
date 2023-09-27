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

open Format
open Config
open Clflags
open Misc
open Cmm

module External = struct
  type state = { mutable asm_code : X86_ast.asm_line list }

  type 'state state' =
    | Unavailable : [< `Before_emit | `Waiting_to_be_assembled] state'
    | Available : state -> [`During_emit] state'

  type t =
    | File of
        { name : string;
          keep_asm : bool;
          channel : Out_channel.t
        }
    | Stdout

  let channel = function
    | File { channel; name; _ } -> channel
    | Stdout -> stdout

  let cleanup = function
    | File { channel; name; keep_asm } ->
      if not keep_asm then Misc.remove_file name;
      close_out channel
    | Stdout -> ()

  let begin_emit t Unavailable =
    Emitaux.output_channel := channel t;
    let data = { asm_code = [] } in
    data, Available data

  let end_emit t (Available { asm_code }) =
    Emitaux.Dwarf_helpers.emit_dwarf ();
    let asm = List.rev asm_code in
    let generate =
      if X86_proc.masm then X86_masm.generate_asm else X86_gas.generate_asm
    in
    Profile.record ~accumulate:true "write_asm" (generate (channel t)) asm;
    Emitaux.output_channel := stderr;
    Unavailable

  let assemble ~obj_filename target Unavailable =
    match target with
    | File { name; channel; _ } ->
      close_out channel;
      Profile.record "assemble" (Proc.assemble_file name) obj_filename
    | Stdout -> 0

  module For_x86 = struct
    let directive state dir = state.asm_code <- dir :: state.asm_code
  end
end

type internal_section = X86_section_name.Tbl.key * X86_ast.asm_line list

type internal_emitter =
  delayed:(unit -> internal_section list) ->
  internal_section list ->
  string ->
  unit

module Internal = struct
  type state =
    { f : internal_emitter;
      asm_code_current_section : X86_ast.asm_line list ref ref;
      asm_code_by_section : X86_ast.asm_line list ref X86_section_name.Tbl.t;
      delayed_sections : X86_ast.asm_line list ref X86_section_name.Tbl.t
    }

  type 'state state' =
    | Registered : internal_emitter -> [`Before_emit] state'
    | Pending : state -> [< `During_emit] state'
    | Done : (string -> unit) -> [`Waiting_to_be_assembled] state'

  type t = unit

  let begin_emit () (Registered f) =
    let data =
      { f;
        asm_code_current_section = ref (ref []);
        asm_code_by_section = X86_section_name.Tbl.create 100;
        delayed_sections = X86_section_name.Tbl.create 100
      }
    in
    data, Pending data

  let end_emit () (Pending { f; asm_code_by_section; delayed_sections; _ }) =
    let get sections =
      X86_section_name.Tbl.fold
        (fun name instrs acc -> (name, List.rev !instrs) :: acc)
        sections []
    in
    let instrs = get asm_code_by_section in
    let delayed () = get delayed_sections in
    Done (f ~delayed instrs)

  let assemble ~obj_filename () (Done f) =
    f obj_filename;
    0

  let cleanup _ = ()

  module For_x86 = struct
    let directive state dir =
      match dir with
      | X86_ast.Section (name, flags, args, is_delayed) -> (
        let name = X86_section_name.make name flags args in
        let where =
          if is_delayed
          then state.delayed_sections
          else state.asm_code_by_section
        in
        match X86_section_name.Tbl.find_opt where name with
        | Some x -> state.asm_code_current_section := x
        | None ->
          state.asm_code_current_section := ref [];
          X86_section_name.Tbl.add where name !(state.asm_code_current_section))
      | dir ->
        !(state.asm_code_current_section)
        := dir :: !(!(state.asm_code_current_section))
  end
end

type global_data =
  | Gerror_if_accessed
  | Gdo_not_emit
  | Gexternal of External.state ref
  | Ginternal of Internal.state ref

let global_data = ref Gerror_if_accessed

type 'state t =
  | Do_not_emit : 'state t
  | Internal : Internal.t * 'state Internal.state' -> 'state t
  | External : External.t * 'state External.state' -> 'state t

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
    let data, state = External.begin_emit target state in
    global_data := Gexternal (ref data);
    External (target, state)
  | Internal (target, state) ->
    let data, state = Internal.begin_emit target state in
    global_data := Ginternal (ref data);
    Internal (target, state)

let end_emit (t : during_emit) =
  let t =
    match t with
    | External (target, state) ->
      let state = External.end_emit target state in
      (External (target, state) : waiting_to_be_assembled)
    | Internal (target, state) ->
      let state = Internal.end_emit target state in
      (Internal (target, state) : waiting_to_be_assembled)
    | Do_not_emit -> Do_not_emit
  in
  global_data := Gerror_if_accessed;
  t

let assemble ~obj_filename = function
  | External (target, state) -> External.assemble ~obj_filename target state
  | Internal (target, state) -> Internal.assemble ~obj_filename target state
  | Do_not_emit -> 0

let cleanup = function
  | Do_not_emit -> ()
  | External (target, _) -> External.cleanup target
  | Internal (target, _) -> Internal.cleanup target

module For_x86 = struct
  let directive dir =
    match !global_data with
    | Gdo_not_emit -> ()
    | Gexternal state -> External.For_x86.directive !state dir
    | Ginternal state -> Internal.For_x86.directive !state dir
    | Gerror_if_accessed -> assert false

  let emit ins = directive (Ins ins)
end
