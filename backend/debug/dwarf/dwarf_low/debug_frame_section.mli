(** Representation of the DWARF .debug_frame section. *)

type t

val create : unit -> t

include Dwarf_emittable.S with type t := t
