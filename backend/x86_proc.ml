(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open X86_ast

module Section_name = struct
  module S = struct
    type t =
      { name: string list;
        name_str: string;
        flags : string option;
        args: string list
      }

    let equal t1 t2 =
      List.equal String.equal t1.name t2.name

    let hash t = Hashtbl.hash t.name

    let compare t1 t2 = List.compare String.compare t1.name t2.name

    let make name flags args =
      { name; name_str = String.concat "," name; flags; args; }

    let of_string name =
      { name = [name]; name_str = name; flags = None; args = [] }

    let to_string t = t.name_str

    let flags t =
      t.flags

    let alignment t =
      let rec align = function
        | [] -> 0L
        | [hd] -> Option.value ~default:0L (Int64.of_string_opt hd)
        | hd :: tl -> align tl
      in align t.args

    let is_text_like t = String.starts_with ~prefix:".text" t.name_str
    let is_data_like t = String.starts_with ~prefix:".data" t.name_str
    let is_note_like t = String.starts_with ~prefix:".note" t.name_str
  end
  include S
  module Map = Map.Make (S)
  module Tbl = Hashtbl.Make (S)
end

type system =
  (* 32 bits and 64 bits *)
  | S_macosx
  | S_gnu
  | S_cygwin

  (* 32 bits only *)
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw

  (* 64 bits only *)
  | S_win64
  | S_linux
  | S_mingw64

  | S_unknown


let system = match Config.system with
  | "macosx" -> S_macosx
  | "solaris" -> S_solaris
  | "win32" -> S_win32
  | "linux_elf" -> S_linux_elf
  | "bsd_elf" -> S_bsd_elf
  | "beos" -> S_beos
  | "gnu" -> S_gnu
  | "cygwin" -> S_cygwin
  | "mingw" -> S_mingw
  | "mingw64" -> S_mingw64
  | "win64" -> S_win64
  | "linux" -> S_linux

  | _ -> S_unknown

let windows =
  match system with
  | S_mingw64 | S_cygwin | S_win64 -> true
  | _ -> false

let string_of_substring_literal k n s =
  let b = Buffer.create (n + 2) in
  let last_was_escape = ref false in
  for i = k to k + n - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.bprintf b "\\%o" (Char.code c)
      else Buffer.add_char b c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      Buffer.add_char b c;
      last_was_escape := false
    end else begin
      Printf.bprintf b "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  Buffer.contents b

let string_of_string_literal s =
  string_of_substring_literal 0 (String.length s) s

let string_of_symbol prefix s =
  let spec = ref false in
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
    | _ -> spec := true;
  done;
  if not !spec then if prefix = "" then s else prefix ^ s
  else
    let b = Buffer.create (String.length s + 10) in
    Buffer.add_string b prefix;
    String.iter
      (function
        | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char b c
        | c -> Printf.bprintf b "$%02x" (Char.code c)
      )
      s;
    Buffer.contents b

let string_of_prefetch_temporal_locality_hint = function
  | Nta -> "nta"
  | T2 -> "t2"
  | T1 -> "t1"
  | T0 -> "t0"

let buf_bytes_directive b directive s =
  let pos = ref 0 in
  for i = 0 to String.length s - 1 do
    if !pos = 0
    then begin
      if i > 0 then Buffer.add_char b '\n';
      Buffer.add_char b '\t';
      Buffer.add_string b directive;
      Buffer.add_char b '\t';
    end
    else Buffer.add_char b ',';
    Printf.bprintf b "%d" (Char.code s.[i]);
    incr pos;
    if !pos >= 16 then begin pos := 0 end
  done

let string_of_reg64 = function
  | RAX -> "rax"
  | RBX -> "rbx"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | RBP -> "rbp"
  | RSP -> "rsp"
  | R8  -> "r8"
  | R9  -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let string_of_reg8l = function
  | RAX -> "al"
  | RBX -> "bl"
  | RCX -> "cl"
  | RDX -> "dl"
  | RSP -> "spl"
  | RBP -> "bpl"
  | RSI -> "sil"
  | RDI -> "dil"
  | R8  -> "r8b"
  | R9  -> "r9b"
  | R10 -> "r10b"
  | R11 -> "r11b"
  | R12 -> "r12b"
  | R13 -> "r13b"
  | R14 -> "r14b"
  | R15 -> "r15b"

let string_of_reg8h = function
  | AH -> "ah"
  | BH -> "bh"
  | CH -> "ch"
  | DH -> "dh"

let string_of_reg16 = function
  | RAX -> "ax"
  | RBX -> "bx"
  | RCX -> "cx"
  | RDX -> "dx"
  | RSP -> "sp"
  | RBP -> "bp"
  | RSI -> "si"
  | RDI -> "di"
  | R8  -> "r8w"
  | R9  -> "r9w"
  | R10 -> "r10w"
  | R11 -> "r11w"
  | R12 -> "r12w"
  | R13 -> "r13w"
  | R14 -> "r14w"
  | R15 -> "r15w"

let string_of_reg32 = function
  | RAX -> "eax"
  | RBX -> "ebx"
  | RCX -> "ecx"
  | RDX -> "edx"
  | RSP -> "esp"
  | RBP -> "ebp"
  | RSI -> "esi"
  | RDI -> "edi"
  | R8  -> "r8d"
  | R9  -> "r9d"
  | R10 -> "r10d"
  | R11 -> "r11d"
  | R12 -> "r12d"
  | R13 -> "r13d"
  | R14 -> "r14d"
  | R15 -> "r15d"

let string_of_regf = function
  | XMM n -> Printf.sprintf "xmm%d" n

let string_of_condition = function
  | E -> "e"
  | AE -> "ae"
  | A -> "a"
  | GE -> "ge"
  | G -> "g"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | L -> "l"
  | LE -> "le"
  | NP -> "np"
  | P -> "p"
  | NS -> "ns"
  | S -> "s"
  | NO -> "no"
  | O -> "o"

let string_of_float_condition = function
  | EQf -> "eq"
  | LTf -> "lt"
  | LEf -> "le"
  | UNORDf -> "unord"
  | NEQf -> "neq"
  | NLTf -> "nlt"
  | NLEf -> "nle"
  | ORDf -> "ord"

let string_of_rounding = function
  | RoundDown -> "roundsd.down"
  | RoundUp -> "roundsd.up"
  | RoundTruncate -> "roundsd.trunc"
  | RoundNearest -> "roundsd.near"
  | RoundCurrent -> "roundsd"

(* Control fields for [roundsd] operation is specified as a 4-bit immediate:
   bit 3: whether to signal Precision Floating-Point Exception.
   bit 2: if set, select rounding mode from MXCSR.RC, else use bits 0 and 1.
   bits 0 and 1: rounding mode, according to  Table 4-17 of
   Intel® 64 and IA-32 Architectures Software Developer’s Manual Volume 2. *)
let imm_of_rounding = function
  | RoundNearest -> Imm 8L
  | RoundDown -> Imm 9L
  | RoundUp -> Imm 10L
  | RoundTruncate -> Imm 11L
  | RoundCurrent -> Imm 12L

let internal_assembler = ref None
let register_internal_assembler f = internal_assembler := Some f

(* Which asm conventions to use *)
let masm =
  match system with
  | S_win32 | S_win64 -> true
  | _ -> false

let use_plt =
  match system with
  | S_macosx | S_mingw64 | S_cygwin | S_win64 -> false
  | _ -> !Clflags.dlcode

type output =
  | Main
  | Split_dwarf

(* Shall we use an external assembler command ?
   If [binary_content] contains some data, we can directly
   save it. Otherwise, we have to ask an external command.
*)
let binary_content =
  let main = ref None in
  let dwarf = ref None in
  function
  | Main -> main
  | Split_dwarf -> dwarf

let compile infile outfile =
  if masm then
    Ccomp.command (Config.asm ^
                   Filename.quote outfile ^ " " ^ Filename.quote infile ^
                   (if !Clflags.verbose then "" else ">NUL"))
  else
    Ccomp.command (Config.asm ^ " " ^
                   (String.concat " " (Misc.debug_prefix_map_flags ())) ^
                   " -o " ^ Filename.quote outfile ^ " " ^
                   Filename.quote infile)

let assemble_file output infile outfile =
  match !(binary_content output) with
  | None -> compile infile outfile
  | Some content -> content outfile; (binary_content output) := None; 0

module State = struct
  type t = {
    mutable asm_code : X86_ast.asm_line list ;
    mutable asm_code_current_section : X86_ast.asm_line list ref;
    mutable asm_code_by_section : X86_ast.asm_line list ref Section_name.Tbl.t;
    mutable delayed_sections : X86_ast.asm_line list ref Section_name.Tbl.t;
  }
  let create () =
    { asm_code = [];
      asm_code_current_section = ref [];
      asm_code_by_section = Section_name.Tbl.create 100;
      delayed_sections = Section_name.Tbl.create 100 }

  let assign ~value to_ =
    to_.asm_code <- value.asm_code;
    to_.asm_code_current_section <- value.asm_code_current_section;
    to_.asm_code_by_section <- value.asm_code_by_section;
    to_.delayed_sections <- value.delayed_sections

  let reset t =
    t.asm_code <- [];
    t.asm_code_current_section <- ref [];
    Section_name.Tbl.clear t.asm_code_by_section

end

let main_state = State.create ()
let split_dwarf_state = State.create ()
let state = State.create ()

let current_output = ref Main

let switch_to_split_dwarf () =
  match !current_output with
  | Split_dwarf -> ()
  | Main ->
    State.assign ~value:state main_state;
    State.assign ~value:split_dwarf_state state

let switch_to_binary () =
  match !current_output with
  | Main -> ()
  | Split_dwarf ->
    State.assign ~value:state split_dwarf_state;
    State.assign ~value:main_state state

(* Cannot use Emitaux directly here or there would be a circular dep *)
let create_asm_file = ref true

let directive dir =
  (if !create_asm_file then
     state.asm_code <- dir :: state.asm_code);
  match dir with
  | Section (name, flags, args, is_delayed) -> (
      let name = Section_name.make name flags args in
      let where = if is_delayed then state.delayed_sections else state.asm_code_by_section in
      match Section_name.Tbl.find_opt where name with
      | Some x -> state.asm_code_current_section <- x
      | None ->
        state.asm_code_current_section <- ref [];
        Section_name.Tbl.add where name state.asm_code_current_section)
  | dir -> state.asm_code_current_section := dir :: !(state.asm_code_current_section)

let emit ins = directive (Ins ins)

let output_channel_split_dwarf = ref stdout

let output_channel_main = ref stdout

let reset_asm_code () =
  State.reset state;
  State.reset split_dwarf_state;
  State.reset main_state;
  current_output := Main

let switch_to_output = function
  | Split_dwarf -> switch_to_split_dwarf ()
  | Main -> switch_to_binary ()

let generate_code output asm =
  switch_to_output output;
  begin match asm with
  | Some f ->
    let channel =
      match !current_output with
      | Split_dwarf -> !output_channel_split_dwarf
      | Main -> !output_channel_main
    in
    Profile.record ~accumulate:true "write_asm" (f channel) (List.rev state.asm_code)
  | None -> ()
  end;
  begin match !internal_assembler with
    | Some f ->
      let get sections =
         Section_name.Tbl.fold (fun name instrs acc ->
            (name, List.rev !instrs) :: acc)
          sections []
      in
      let instrs = get state.asm_code_by_section in
      let delayed () = get state.delayed_sections in
      binary_content output := Some (f ~delayed instrs)
  | None -> binary_content output := None
  end
