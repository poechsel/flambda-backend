(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Combine heap allocations occurring in the same basic block *)

open Mach

type pending_alloc =
  { reg: Reg.t;         (* register holding the result of the last allocation *)
    dbginfos: Debuginfo.alloc_dbginfo;   (* debug info for each pending alloc *)
    totalsz: int }                    (* amount to be allocated in this block *)

type mode_allocation_state =
    No_alloc
  | Pending_alloc of pending_alloc

type allocation_state =
  { heap: mode_allocation_state; local: mode_allocation_state }

let get_mode (m : Lambda.alloc_mode) s =
  match m with
  | Alloc_heap -> s.heap
  | Alloc_local -> s.local

let set_mode (m : Lambda.alloc_mode) s x =
  match m with
  | Alloc_heap -> { s with heap = x }
  | Alloc_local -> { s with local = x }

let rec combine i allocstate =
  match i.desc with
    Iend | Ireturn | Iexit _ | Iraise _ ->
      (i, allocstate)
  | Iop(Ialloc { bytes = sz; dbginfo; mode }) ->
      assert (List.length dbginfo = 1);
      begin match get_mode mode allocstate with
      | Pending_alloc {reg; dbginfos; totalsz}
          when (totalsz + sz <= (Config.max_young_wosize + 1) * Arch.size_addr)
               || mode = Lambda.Alloc_local ->
          let (next, state) =
           combine i.next
             (set_mode mode allocstate
                (Pending_alloc { reg = i.res.(0);
                                 dbginfos = dbginfo @ dbginfos;
                                 totalsz = totalsz + sz })) in
         (instr_cons_debug (Iop(Iintop_imm(Iadd, -sz)))
            [| reg |] i.res i.dbg next,
           state)
      | No_alloc | Pending_alloc _ ->
         let (next, state) =
           combine i.next
             (set_mode mode allocstate
                (Pending_alloc { reg = i.res.(0);
                                 dbginfos = dbginfo;
                                 totalsz = sz })) in
         let totalsz, dbginfo =
           match get_mode mode state with
           | No_alloc -> assert false
           | Pending_alloc { totalsz; dbginfos; _ } -> totalsz, dbginfos in
         let next =
           let offset = totalsz - sz in
           if offset = 0 then next
           else instr_cons_debug (Iop(Iintop_imm(Iadd, offset))) i.res
                i.res i.dbg next
         in
         (instr_cons_debug (Iop(Ialloc {bytes = totalsz; dbginfo; mode}))
          i.arg i.res i.dbg next, allocstate)
      end
  | Iop(Icall_ind | Icall_imm _ | Iextcall _ |
        Itailcall_ind | Itailcall_imm _) ->
      let newnext = combine_restart i.next in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext,
       allocstate)
  | Iop _ ->
      let (newnext, s') = combine i.next allocstate in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext, s')
  | Iifthenelse(test, ifso, ifnot) ->
      let newifso = combine_restart ifso in
      let newifnot = combine_restart ifnot in
      let newnext = combine_restart i.next in
      (instr_cons (Iifthenelse(test, newifso, newifnot)) i.arg i.res newnext,
       allocstate)
  | Iswitch(table, cases) ->
      let newcases = Array.map combine_restart cases in
      let newnext = combine_restart i.next in
      (instr_cons (Iswitch(table, newcases)) i.arg i.res newnext,
       allocstate)
  | Icatch(rec_flag, handlers, body) ->
      let (newbody, s') = combine body allocstate in
      let newhandlers =
        List.map (fun (io, handler) -> io, combine_restart handler) handlers in
      let newnext = combine_restart i.next in
      (instr_cons (Icatch(rec_flag, newhandlers, newbody))
         i.arg i.res newnext, s')
  | Itrywith(body, handler) ->
      let (newbody, s') = combine body allocstate in
      let newhandler = combine_restart handler in
      let newnext = combine_restart i.next in
      (instr_cons (Itrywith(newbody, newhandler)) i.arg i.res newnext, s')

and combine_restart i =
  let (newi, _) = combine i {local=No_alloc; heap=No_alloc} in newi

let fundecl f =
  {f with fun_body = combine_restart f.fun_body}
