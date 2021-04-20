(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module STbl = Misc.String.Tbl

(* Mapping from basenames to full filenames *)
type registry = string STbl.t

open Local_store

let files : registry ref = s_table STbl.create 42
let files_uncap : registry ref = s_table STbl.create 42

module Dir = struct
  type t = {
    path : string;
    files : string list;
  }

  let path t = t.path
  let files t = t.files

  let create path =
    { path; files = Array.to_list (Directory_content_cache.read path) }
end

let dirs = s_ref []

let reset () =
  assert (Local_store.is_bound ());
  STbl.clear !files;
  STbl.clear !files_uncap;
  dirs := []

let get () = !dirs
let get_paths () = List.map Dir.path !dirs

let add dir =
  assert (Local_store.is_bound ());
  let add_file base =
    let fn = Filename.concat dir.Dir.path base in
    STbl.replace !files base fn;
    STbl.replace !files_uncap (String.uncapitalize_ascii base) fn
  in
  List.iter add_file dir.Dir.files;
  dirs := dir :: !dirs

let remove_dir dir =
  assert (Local_store.is_bound ());
  let new_dirs = List.filter (fun d -> Dir.path d <> dir) !dirs in
  if new_dirs <> !dirs then begin
    reset ();
    List.iter add (List.rev new_dirs)
  end

let add_dir dir = add (Dir.create dir)

let init l =
  reset ();
  List.iter add_dir (List.rev l)

let is_basename fn = Filename.basename fn = fn

let find fn =
  assert (Local_store.is_bound ());
  if is_basename fn then begin
    STbl.find !files fn
  end else begin
    Misc.find_in_path (get_paths ()) fn
  end

let find_uncap fn =
  assert (Local_store.is_bound ());
  if is_basename fn then begin
    STbl.find !files_uncap (String.uncapitalize_ascii fn)
  end else begin
    Misc.find_in_path_uncap (get_paths ()) fn
  end
