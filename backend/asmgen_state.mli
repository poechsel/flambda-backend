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

type 'state t

type before_emit = [`Before_emit] t

type during_emit = [`During_emit] t

type waiting_to_be_assembled = [`Waiting_to_be_assembled] t


type internal_section = X86_section_name.Tbl.key * X86_ast.asm_line list

type internal_emitter =
  delayed:(unit -> internal_section list) ->
  internal_section list ->
  string ->
  unit

(* Creates a new state *)
val do_not_emit : [< `Before_emit | `During_emit | `Waiting_to_be_assembled] t

val to_stdout : unit -> before_emit

val using_external_assembler :
  keep_asm:bool -> asm_filename:string -> before_emit

val using_internal_assembler : internal_emitter -> before_emit

(* Manage the state *)
val begin_emit : before_emit -> during_emit

val end_emit : during_emit -> waiting_to_be_assembled

val assemble : obj_filename:string -> waiting_to_be_assembled -> int

(* Cleanup functions *)
val cleanup : _ t -> unit

(* Replaces X86_proc *)
module For_x86 : sig
  val emit : X86_ast.instruction -> unit

  val directive : X86_ast.asm_line -> unit
end
