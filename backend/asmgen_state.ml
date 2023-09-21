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

let do_not_emit = Do_not_emit

let create () = Emit

let begin_emit = function Emit -> Emit | Do_not_emit -> Do_not_emit

let end_emit = function Emit -> Emit | Do_not_emit -> Do_not_emit

let assemble ~asm_filename ~obj_filename = function
  | Emit ->
    Profile.record "assemble" (Proc.assemble_file asm_filename) obj_filename
  | Do_not_emit -> 0
