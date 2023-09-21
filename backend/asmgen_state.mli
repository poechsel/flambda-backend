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

type 'state t =
  | Do_not_emit : 'state t
  | Emit : 'state t

type before_emit = [`Before_emit] t

type during_emit = [`During_emit] t

type waiting_to_be_assembled = [`Waiting_to_be_assembled] t

val do_not_emit : [< `Before_emit | `During_emit | `Waiting_to_be_assembled] t

val create : unit -> before_emit

val begin_emit : before_emit -> during_emit

val end_emit : during_emit -> waiting_to_be_assembled

val assemble :
  asm_filename:string -> obj_filename:string -> waiting_to_be_assembled -> int
