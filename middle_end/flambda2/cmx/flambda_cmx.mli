(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Dumping and restoring of simplification environment information to and from
    .cmx files. *)

type loader

val create_loader : (module Flambda_backend_intf.S) -> loader

val get_imported_names : loader -> unit -> Name.Set.t

val get_imported_code : loader -> unit -> Exported_code.t

val load_cmx_file_contents :
  loader -> Compilation_unit.t -> Flambda_type.Typing_env.t option

val prepare_cmx_file_contents :
  return_cont_env:Continuation_uses_env.t ->
  return_continuation:Continuation.t ->
  module_symbol:Symbol.t ->
  used_closure_vars:Var_within_closure.Set.t ->
  Exported_code.t ->
  Flambda_cmx_format.t option
