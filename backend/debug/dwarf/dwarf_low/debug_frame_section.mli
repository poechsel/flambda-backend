(** Representation of the DWARF .debug_frame section. *)

type t

val create : code_begin:Asm_targets.Asm_symbol.t -> t

include Dwarf_emittable.S with type t := t
