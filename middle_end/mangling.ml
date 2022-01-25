(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2021--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let begins_with ?(from = 0) str prefix =
  let rec helper idx =
    if idx < 0 then true
    else
      String.get str (from + idx) = String.get prefix idx && helper (idx-1)
  in
  let n = String.length str in
  let m = String.length prefix in
  if n >= from + m then helper (m-1) else false

let split_on_string str split =
  let n = String.length str in
  let m = String.length split in
  let rec helper acc last_idx idx =
    if idx = n then
      let cur = String.sub str last_idx (idx - last_idx) in
      List.rev (cur :: acc)
    else if begins_with ~from:idx str split then
      let cur = String.sub str last_idx (idx - last_idx) in
      helper (cur :: acc) (idx + m) (idx + m)
    else
      helper acc last_idx (idx + 1)
  in
  helper [] 0 0

let split_on_chars str chars =
  let rec helper chars_left s acc =
    match chars_left with
    | [] -> s :: acc
    | c :: cs ->
      List.fold_right (helper cs) (String.split_on_char c s) acc
  in
  helper chars str []

let split_last_exn str c =
  let n = String.length str in
  let ridx = String.rindex str c in
  (String.sub str 0 ridx, String.sub str (ridx + 1) (n - ridx - 1))

let escape_symbols part =
  let buf = Buffer.create 16 in
  let was_hex_last = ref false in
  let handle_char = function
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c ->
      if !was_hex_last then Buffer.add_string buf "__";
      Buffer.add_char buf c;
      was_hex_last := false
    | c ->
      Printf.bprintf buf "%sX%02x"
        (if !was_hex_last then "_" else "__")
        (Char.code c);
      was_hex_last := true
  in
  String.iter handle_char part;
  Buffer.contents buf

type expression =
  | String of string
  | Dot of expression * string

type cpp_name =
  | Simple of string
  | Scoped of cpp_name list
  | Templated of string * template_arg list

and template_arg =
  | Cpp_name of cpp_name
  | Expression of expression

let mangle_cpp name =
  let with_length s =
    let s = escape_symbols s in
    Printf.sprintf "%d%s" (String.length s) s in
  let rec mangle_expression = function
    | String s -> with_length s
    | Dot (e, name) -> Printf.sprintf "dt%s%s" (mangle_expression e) (with_length name)
  in
  let rec mangle_name = function
    | Simple s -> with_length s
    | Scoped names ->
      let s = List.map mangle_name names |> String.concat "" in
      Printf.sprintf "N%sE" s
    | Templated (str, parts) ->
      let s = List.map mangle_arg parts |> String.concat "" in
      Printf.sprintf "%sI%sE" (with_length str) s
  and mangle_arg = function
    | Cpp_name name -> mangle_name name
    | Expression expression ->
      Printf.sprintf "X%sE" (mangle_expression expression)
  in
  "_Z" ^ mangle_name name

let file_template_arg file =
  (* Take the file name only *)
  let filename =
    if String.contains file '/' then snd (split_last_exn file '/')
    else file
  in
  match String.split_on_char '.' filename with
  | [] -> Misc.fatal_error "Empty split"
  | hd :: tl ->
    let expr = List.fold_left (fun e x -> Dot (e, x)) (String hd) tl in
    Expression expr

let name_op = function
  | "+" -> "PLUS"
  | "++" -> "PLUSPLUS"
  | "+." -> "PLUSDOT"
  | "+=" -> "PLUSEQ"
  | "-" -> "MINUS"
  | "-." -> "MINUSDOT"
  | "*" -> "STAR"
  | "%" -> "PERCENT"
  | "=" -> "EQUAL"
  | "<" -> "LESS"
  | ">" -> "GREATER"
  | "<>" -> "NOTEQUAL"
  | "||" -> "BARBAR"
  | "&" -> "AMPERSAND"
  | "&&" -> "AMPERAMPER"
  | ":=" -> "COLONEQUAL"
  | "^" -> "CARET"
  | "^^" -> "CARETCARET"
  | "@" -> "AT"
  | "<<" -> "LSHIFT"
  | ">>" -> "RSHIFT"
  | op -> op

let build_location_info loc =
  let loc = Debuginfo.Scoped_location.to_location loc in
  let (file, line, startchar) = Location.get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in
  let line_str = Printf.sprintf "ln_%d" line in
  let info = [ file_template_arg file; Cpp_name (Simple line_str) ] in
  if startchar >= 0 then
    let char_str = Printf.sprintf "ch_%d_to_%d" startchar endchar in
    info @ [ Cpp_name (Simple char_str) ]
  else info

(* OCaml names can contain single quotes but need to be escaped
   for C++ identifiers. *)
let convert_identifier str =
  match String.split_on_char '\'' str with
  | [] -> Misc.fatal_error "empty split"
  | [ s ] -> Simple s
  | parts ->
    let s = String.concat "_Q" parts in
    Templated (s, [ Cpp_name (Simple "quoted")] )

let convert_closure_id id loc =
  if begins_with id "anon_fn[" then
    (* Keep the unique integer stamp *)
    let (_init, stamp) = split_last_exn id '_' in
    (* Put the location inside C++ template args *)
    Templated ("anon_fn_" ^ stamp, build_location_info loc)
  else
    match id.[0] with
    (* A regular identifier *)
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') -> convert_identifier id
    (* An operator *)
    | _op ->
      let (op, stamp) = split_last_exn id '_' in
      Templated ("op_" ^ stamp, [Cpp_name (Simple (name_op op))])
 
let convert_scope scope =
  let n = String.length scope in
  (* anonymous function *)
  if String.equal scope "(fun)" then Templated ("anon_fn", [])
  (* operators *)
  else if n > 2 && String.get scope 0 = '(' && String.get scope (n - 1) = ')' then
    let op = String.sub scope 1 (n - 2) in
    Templated ("op", [ Cpp_name (Simple (name_op op)) ])
  (* regular identifiers *)
  else convert_identifier scope

let list_of_scopes scopes =
  (* Works for now since the only separators are '.' and '#' *)
  let scope_str = Debuginfo.Scoped_location.string_of_scopes scopes in
  split_on_chars scope_str [ '.'; '#' ]

let scope_matches_closure_id scope closure_id =
  (* If the `id` is an anonymous function this corresponds to that,
      and, even if not, then the function has likely been given
      a name via some aliasing (e.g. `let f = fun x -> ...`) *)
  String.equal scope "(fun)" ||
  (* Normal case where closure id and scope match directly *)
  begins_with closure_id scope ||
  (* For operators, the scope is wrapped in parens *)
  ( String.length scope >= 3 &&
    begins_with closure_id (String.sub scope 1 (String.length scope - 2)))

(* Returns a pair of the top-level module and the list of scopes
   the strictly contain the closure id *)
let module_and_scopes ~unitname loc id =
  match (loc : Debuginfo.Scoped_location.t) with
  | Loc_known { loc = _; scopes } ->
    let scopes = list_of_scopes scopes in
    (* Remove last scope if it matches closure id *)
    let scopes =
      match List.rev scopes with
      | [] -> Misc.fatal_errorf "No location - %s %s" unitname id
      | last_scope :: rest when scope_matches_closure_id last_scope id ->
        List.rev rest
      | _ -> scopes
    in
    (* If the scope is now empty, use the unitname as the top-level module *)
    begin match scopes with
      | [] -> unitname, []
      | top_module :: sub_scopes -> top_module, sub_scopes
    end
  | Loc_unknown -> unitname, []

let remove_prefix ~prefix str =
  let n = String.length prefix in
  if begins_with str prefix then
    String.sub str n (String.length str - n)
  else
    str

let fun_symbol ~unitname ~loc ~id =
  let unitname = remove_prefix ~prefix:"caml" unitname  in
  let top_level_module, sub_scopes = module_and_scopes ~unitname loc id in
  let namespace_parts name =
    split_on_string name "__" |> List.map (fun part -> Simple part)
  in
  let parts =
    List.concat [
      namespace_parts top_level_module;
      List.map convert_scope sub_scopes;
      [ convert_closure_id id loc ];
      if String.equal top_level_module unitname then []
      else [
        Templated ("inlined_in",
          [ Cpp_name (Scoped (namespace_parts unitname)) ])
      ]
    ]
  in
  mangle_cpp (Scoped parts)
