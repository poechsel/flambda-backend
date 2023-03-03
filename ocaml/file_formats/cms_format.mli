(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** cms and cmsi files format. *)

(** The layout of a cms file is as follows:
      <cms> := \{<cmi>\} <cms magic> \{cms infos\} \{<source info>\}
    where <cmi> is the cmi file format:
      <cmi> := <cmi magic> <cmi info>.
    More precisely, the optional <cmi> part must be present if and only if
    the file is:
    - a cmsi, or
    - a cms, for a ml file which has no corresponding mli (hence no
    corresponding cmsi).

    Thus, we provide a common reading function for cmi and cms(i)
    files which returns an option for each of the three parts: cmi
    info, cms info, source info. *)

open Typedtree

type item_declaration =
  | Class_declaration of class_declaration
  | Class_description of class_description
  | Class_type_declaration of class_type_declaration
  | Extension_constructor of extension_constructor
  | Module_binding of module_binding
  | Module_declaration of module_declaration
  | Module_type_declaration of module_type_declaration
  | Type_declaration of type_declaration
  | Value_binding of value_binding
  | Value_description of value_description

type cms_infos = {
  cms_modname : Compilation_unit.t;
  cms_comments : (string * Location.t) list;
  cms_args : string array;
  cms_sourcefile : string option;
  cms_builddir : string;
  cms_loadpath : string list;
  cms_source_digest : string option;
  cms_interface_digest : Digest.t option;
  cms_uid_to_attributes : attributes Shape.Uid.Tbl.t;
  cms_impl_shape : Shape.t option; (* None for mli *)
}

type error =
    Not_a_shape of string

exception Error of error

(** [read filename] opens filename, and extract both the cmi_infos, if
    it exists, and the cms_infos, if it exists. Thus, it can be used
    with .cmi, .cms and .cmsi files.

    .cmsi files always contain a cmi_infos at the beginning. .cms files
    only contain a cmi_infos at the beginning if there is no associated
    .cmsi file.
*)
val read : string -> Cmi_format.cmi_infos option * cms_infos option

val read_cms : string -> cms_infos
val read_cmi : string -> Cmi_format.cmi_infos

(** [save_cms filename modname binary_annots sourcefile initial_env cmi]
    writes a cms(i) file.  *)
val save_cms :
  string ->  (* filename.cms to generate *)
  Compilation_unit.t ->  (* module name *)
  Cmt_format.binary_annots ->
  string option ->  (* source file *)
  Cmi_format.cmi_infos option -> (* if a .cmi was generated *)
  Shape.t option ->
  unit

(* Miscellaneous functions *)

val read_magic_number : in_channel -> string