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


(** Definitions shared between the 32 and 64 bit Intel backends. *)

open X86_ast

(** Helpers for textual emitters *)

val string_of_reg8l: reg64 -> string
val string_of_reg8h: reg8h -> string
val string_of_reg16: reg64 -> string
val string_of_reg32: reg64 -> string
val string_of_reg64: reg64 -> string
val string_of_regf: regf -> string
val string_of_substring_literal: int -> int -> string -> string
val string_of_string_literal: string -> string
val string_of_condition: condition -> string
val string_of_float_condition: float_condition -> string
val string_of_symbol: (*prefix*) string -> string -> string
val string_of_rounding: rounding -> string
val imm_of_rounding: rounding -> arg
val string_of_prefetch_temporal_locality_hint:
  prefetch_temporal_locality_hint -> string
val buf_bytes_directive:
  Buffer.t -> (*directive*) string -> (*data*)string -> unit

(** System detection *)

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

val system: system
val masm: bool
val windows:bool

(** Whether calls need to go via the PLT. *)
val use_plt : bool

val compile: string -> string -> int
