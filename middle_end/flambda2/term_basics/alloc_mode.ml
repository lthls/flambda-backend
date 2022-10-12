(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Must_be_heap
  | May_be_local

type without_region = t

let print ppf t =
  match t with
  | Must_be_heap -> Format.pp_print_string ppf "Must_be_heap"
  | May_be_local -> Format.pp_print_string ppf "May_be_local"

let compare t1 t2 =
  match t1, t2 with
  | Must_be_heap, Must_be_heap | May_be_local, May_be_local -> 0
  | Must_be_heap, May_be_local -> -1
  | May_be_local, Must_be_heap -> 1

let must_be_heap = Must_be_heap

let may_be_local () =
  if Flambda_features.stack_allocation_enabled () then May_be_local else Must_be_heap

let from_lambda (mode : Lambda.alloc_mode) =
  if not (Flambda_features.stack_allocation_enabled ())
  then Must_be_heap
  else match mode with Alloc_heap -> Must_be_heap | Alloc_local -> May_be_local

let to_lambda t =
  match t with
  | Must_be_heap -> Lambda.alloc_heap
  | May_be_local ->
    assert (Flambda_features.stack_allocation_enabled ());
    Lambda.alloc_local

module With_region = struct
  type t =
    | Must_be_heap
    | May_be_local of { region : Variable.t }

  let print ppf t =
    match t with
    | Must_be_heap -> Format.pp_print_string ppf "Must_be_heap"
    | May_be_local { region } ->
      Format.fprintf ppf "@[<hov 1>(May_be_local (region@ %a))@]" Variable.print region

  let compare t1 t2 =
    match t1, t2 with
    | Must_be_heap, Must_be_heap -> 0
    | May_be_local { region = region1 }, May_be_local { region = region2 } ->
      Variable.compare region1 region2
    | Must_be_heap, May_be_local _ -> -1
    | May_be_local _, Must_be_heap -> 1

  let must_be_heap = Must_be_heap

  let may_be_local ~region =
    if Flambda_features.stack_allocation_enabled ()
    then May_be_local { region }
    else Must_be_heap

  let without_region t : without_region =
    match t with Must_be_heap -> Must_be_heap | May_be_local _ -> May_be_local

  let from_lambda (mode : Lambda.alloc_mode) ~current_region =
    if not (Flambda_features.stack_allocation_enabled ())
    then Must_be_heap
    else
      match mode with
      | Alloc_heap -> Must_be_heap
      | Alloc_local -> May_be_local { region = current_region }

  let to_lambda t =
    match t with
    | Must_be_heap -> Lambda.alloc_heap
    | May_be_local _ ->
      assert (Flambda_features.stack_allocation_enabled ());
      Lambda.alloc_local

  let free_names t =
    match t with
    | Must_be_heap -> Name_occurrences.empty
    | May_be_local { region } ->
      Name_occurrences.singleton_variable region Name_mode.normal

  let apply_renaming t renaming =
    match t with
    | Must_be_heap -> Must_be_heap
    | May_be_local { region } ->
      let region' = Renaming.apply_variable renaming region in
      if region == region' then t else May_be_local { region = region' }

  let ids_for_export t =
    match t with
    | Must_be_heap -> Ids_for_export.empty
    | May_be_local { region } -> Ids_for_export.singleton_variable region
end
