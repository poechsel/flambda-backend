open Arch

(* Signed immediates are simpler *)

let is_immediate n =
  let mn = -n in
  n land 0xFFF = n || n land 0xFFF_000 = n
  || mn land 0xFFF = mn || mn land 0xFFF_000 = mn

(* Instruction selection *)

class selector = object(_)

inherit Selectgen.selector_generic as super

method is_immediate_test _cmp n = is_immediate n

method! effects_of e = super#effects_of e
(* CR-someday poechsel: This is a very bad way to handle addressing *)
method select_addressing _ arg = (Iindexed 0, arg)
end

let fundecl ~future_funcnames f = (new selector)#emit_fundecl ~future_funcnames f
