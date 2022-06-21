(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module TE = Typing_env
module TG = Type_grammar

type to_lift =
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        fields : Simple.t list
      }
  | Boxed_float of Float.t
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint_32_64.t
  | Empty_array

type reification_result =
  | Lift of to_lift
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

(* CR mshinwell: Think more to identify all the cases that should be in this
   function. *)
let reify ?allowed_if_free_vars_defined_in ?additional_free_var_criterion
    ?disallowed_free_vars ?(allow_unique = false) env ~min_name_mode t :
    reification_result =
  let var_allowed (alloc_mode : Alloc_mode.t Or_unknown.t) var =
    (* It is only safe to lift a [Local] allocation if it can be guaranteed that
       no locally-allocated value is reachable from it: therefore, any variables
       involved in the definition of an (inconstant) value to be lifted have
       their types checked to ensure they cannot hold locally-allocated values.
       Conversely, [Heap] allocations can be lifted even if inconstant, because
       the OCaml type system will have validated the correctness of the original
       non-lifted terms; any places in the compiler where new [Local] blocks are
       created (e.g. during partial application wrapper expansion) will have
       been checked to ensure they do not break the invariants; and finally
       because the Flambda 2 type system accurately propagates the allocation
       modes (and if it loses information there, we won't lift). *)
    let allowed =
      match allowed_if_free_vars_defined_in with
      | None -> false
      | Some allowed_if_free_vars_defined_in -> (
        TE.mem ~min_name_mode allowed_if_free_vars_defined_in (Name.var var)
        && (match additional_free_var_criterion with
           | None -> true
           | Some criterion -> criterion var)
        &&
        match disallowed_free_vars with
        | None -> true
        | Some disallowed_free_vars ->
          not (Variable.Set.mem var disallowed_free_vars))
    in
    allowed
    &&
    match alloc_mode with
    | Known Heap -> true
    | Unknown | Known Local -> (
      match
        Provers.never_holds_locally_allocated_values env var Flambda_kind.value
      with
      | Proved () -> true
      | Unknown | Wrong_kind -> false)
  in
  let canonical_simple =
    match TE.get_alias_then_canonical_simple_exn env ~min_name_mode t with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  match canonical_simple with
  | Some canonical_simple when Simple.is_symbol canonical_simple ->
    (* Don't lift things that are already bound to symbols. Apart from anything
       else, this could cause aliases between symbols, which are currently
       forbidden (every symbol has the same binding time). *)
    Cannot_reify
  | canonical_simple_opt -> (
    let try_canonical_simple () =
      match canonical_simple_opt with
      | None -> Cannot_reify
      | Some canonical_simple -> Simple canonical_simple
    in
    match
      Expand_head.expand_head env t |> Expand_head.Expanded_type.descr_oub
    with
    | Value (Ok (Variant { is_unique; blocks; immediates; alloc_mode })) -> (
      if is_unique && not allow_unique
      then try_canonical_simple ()
      else
        match blocks, immediates with
        | Known blocks, Known imms ->
          if Expand_head.is_bottom env imms
          then
            match TG.Row_like_for_blocks.get_singleton blocks with
            | None -> try_canonical_simple ()
            | Some ((tag, size), field_types) -> (
              assert (
                Targetint_31_63.Imm.equal size
                  (TG.Product.Int_indexed.width field_types));
              (* CR mshinwell: Could recognise other things, e.g. tagged
                 immediates and float arrays, supported by [Static_part]. *)
              match Tag.Scannable.of_tag tag with
              | None -> try_canonical_simple ()
              | Some tag ->
                let field_types =
                  TG.Product.Int_indexed.components field_types
                in
                let field_simples =
                  List.filter_map
                    (fun field_type : Simple.t option ->
                      match
                        Provers.prove_equals_to_simple_of_kind_value env
                          field_type
                      with
                      | Proved simple
                        when not (Coercion.is_id (Simple.coercion simple)) ->
                        (* CR-someday lmaurer: Support lifting things whose
                           fields have coercions. *)
                        None
                      | Proved simple ->
                        Simple.pattern_match' simple
                          ~var:(fun var ~coercion:_ ->
                            if var_allowed alloc_mode var
                            then Some simple
                            else None)
                          ~symbol:(fun _sym ~coercion:_ -> Some simple)
                          ~const:(fun const ->
                            match Reg_width_const.descr const with
                            | Tagged_immediate _imm -> Some simple
                            | Naked_immediate _ | Naked_float _ | Naked_int32 _
                            | Naked_int64 _ | Naked_nativeint _ ->
                              (* This should never happen, as we should have got
                                 [Wrong_kind] instead *)
                              None)
                      | Unknown | Wrong_kind -> None)
                    field_types
                in
                if List.compare_lengths field_types field_simples = 0
                then
                  Lift
                    (Immutable_block { tag; is_unique; fields = field_simples })
                else try_canonical_simple ())
          else if TG.Row_like_for_blocks.is_bottom blocks
          then
            match Provers.meet_naked_immediates env imms with
            | Known_result imms -> (
              match Targetint_31_63.Set.get_singleton imms with
              | None -> try_canonical_simple ()
              | Some imm ->
                Simple (Simple.const (Reg_width_const.tagged_immediate imm)))
            | Need_meet -> try_canonical_simple ()
            | Invalid -> Invalid
          else try_canonical_simple ()
        | Known _, Unknown | Unknown, Known _ | Unknown, Unknown ->
          try_canonical_simple ())
    | Value (Ok (Mutable_block _)) -> try_canonical_simple ()
    | Value (Ok (Closures { by_function_slot = _; alloc_mode = _ })) ->
      try_canonical_simple ()
      (* CR vlaviron: This rather complicated code could be useful, but since a
         while ago Reification simply ignores List_set_of_closures results. So
         I've commented out the code for now, and if we want to turn it on again
         later we will need to think about issues like code duplication,
         mutually recursive functions, and so on. *)
      (* Example:
       * include (struct
       *   module type T = sig
       *     val n : int
       *   end
       *
       *   module F(T : T) = struct
       *     let f x = x + T.n [@@inline never]
       *   end [@@inline never]
       *
       *   module T = struct let n = 42 end
       *
       *   module A = F(T)
       *
       *   let toto = A.f
       * end :
       * sig
       *   val toto : int -> int
       * end) *)
      (* (\* CR mshinwell: Here and above, move to separate function. *\)
       * match TG.Row_like_for_closures.get_singleton by_function_slot with
       * | None -> try_canonical_simple ()
       * | Some ((function_slot, contents), closures_entry) ->
       *   (\* CR mshinwell: What about if there were multiple entries in the
       *      row-like structure for the same function slot? This is ruled out by
       *      [get_singleton] at the moment. We should probably choose the best
       *      entry from the [Row_like] structure. *\)
       *   let function_slots = Set_of_closures_contents.closures contents in
       *   (\* CR mshinwell: Should probably check
       *      [Set_of_closures_contents.value_slots contents]? *\)
       *   if not (Function_slot.Set.mem function_slot function_slots)
       *   then
       *     Misc.fatal_errorf
       *       "Function slot %a expected in set-of-closures-contents in closure \
       *        type@ %a"
       *       Function_slot.print function_slot TG.print t;
       *   let function_types_with_value_slots =
       *     Function_slot.Set.fold
       *       (fun function_slot function_types_with_value_slots ->
       *         match
       *           TG.Closures_entry.find_function_type closures_entry
       *             function_slot
       *         with
       *         | Bottom | Unknown -> function_types_with_value_slots
       *         | Ok function_type ->
       *           (\* CR mshinwell: We're ignoring [coercion] *\)
       *           let value_slot_types =
       *             TG.Closures_entry.value_slot_types closures_entry
       *           in
       *           let value_slot_simples =
       *             Value_slot.Map.filter_map
       *               (fun _value_slot value_slot_type ->
       *                 match
       *                   Provers
       *                   .prove_equals_to_var_or_symbol_or_tagged_immediate env
       *                     value_slot_type
       *                 with
       *                 | Proved (Var var, coercion) ->
       *                   if var_allowed alloc_mode var
       *                   then
       *                     Some (Simple.with_coercion (Simple.var var) coercion)
       *                   else None
       *                 | Proved (Symbol sym, coercion) ->
       *                   Some (Simple.with_coercion (Simple.symbol sym) coercion)
       *                 | Proved (Tagged_immediate imm, coercion) ->
       *                   Some
       *                     (Simple.with_coercion
       *                        (Simple.const
       *                           (Reg_width_const.tagged_immediate imm))
       *                        coercion)
       *                 | Unknown | Invalid -> None)
       *               value_slot_types
       *           in
       *           if Value_slot.Map.cardinal value_slot_types
       *              <> Value_slot.Map.cardinal value_slot_simples
       *           then function_types_with_value_slots
       *           else
       *             Function_slot.Map.add function_slot
       *               (function_type, value_slot_simples)
       *               function_types_with_value_slots)
       *       function_slots Function_slot.Map.empty
       *   in
       *   if Function_slot.Set.cardinal function_slots
       *      <> Function_slot.Map.cardinal function_types_with_value_slots
       *   then try_canonical_simple ()
       *   else
       *     let function_types =
       *       Function_slot.Map.map
       *         (fun (function_decl, _) -> function_decl)
       *         function_types_with_value_slots
       *     in
       *     let value_slots =
       *       Function_slot.Map.fold
       *         (fun _function_slot (_function_decl, value_slot_simples)
       *              all_value_slots ->
       *           Value_slot.Map.fold
       *             (fun value_slot simple all_value_slots ->
       *               begin
       *                 match Value_slot.Map.find value_slot all_value_slots with
       *                 | exception Not_found -> ()
       *                 | existing_simple ->
       *                   if not (Simple.equal simple existing_simple)
       *                   then
       *                     Misc.fatal_errorf
       *                       "Disagreement on %a and %a (value slot %a)@ whilst \
       *                        reifying set-of-closures from:@ %a"
       *                       Simple.print simple Simple.print existing_simple
       *                       Value_slot.print value_slot TG.print t
       *               end;
       *               Value_slot.Map.add value_slot simple all_value_slots)
       *             value_slot_simples all_value_slots)
       *         function_types_with_value_slots Value_slot.Map.empty
       *     in
       *     Lift_set_of_closures { function_slot; function_types; value_slots } *)
    | Naked_immediate (Ok (Naked_immediates imms)) -> (
      match Targetint_31_63.Set.get_singleton imms with
      | None -> try_canonical_simple ()
      | Some i -> Simple (Simple.const (Reg_width_const.naked_immediate i)))
    | Naked_immediate (Ok (Is_int scrutinee_ty)) -> (
      match Provers.prove_is_int env scrutinee_ty with
      | Proved true -> Simple Simple.untagged_const_true
      | Proved false -> Simple Simple.untagged_const_false
      | Unknown -> try_canonical_simple ()
      | Wrong_kind -> Invalid)
    | Naked_immediate (Ok (Get_tag block_ty)) -> (
      match Provers.prove_get_tag env block_ty with
      | Proved tags -> (
        let is =
          Tag.Set.fold
            (fun tag is ->
              Targetint_31_63.Set.add (Tag.to_targetint_31_63 tag) is)
            tags Targetint_31_63.Set.empty
        in
        match Targetint_31_63.Set.get_singleton is with
        | None -> try_canonical_simple ()
        | Some i -> Simple (Simple.const (Reg_width_const.naked_immediate i)))
      | Unknown -> try_canonical_simple ()
      | Wrong_kind -> Invalid)
    | Naked_float (Ok fs) -> (
      match Float.Set.get_singleton fs with
      | None -> try_canonical_simple ()
      | Some f -> Simple (Simple.const (Reg_width_const.naked_float f)))
    | Naked_int32 (Ok ns) -> (
      match Int32.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int32 n)))
    | Naked_int64 (Ok ns) -> (
      match Int64.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int64 n)))
    | Naked_nativeint (Ok ns) -> (
      match Targetint_32_64.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_nativeint n)))
    (* CR-someday mshinwell: These could lift at toplevel when [ty_naked_float]
       is an alias type. That would require checking the alloc mode. *)
    | Value (Ok (Boxed_float (ty_naked_float, _alloc_mode))) -> (
      match Provers.meet_naked_floats env ty_naked_float with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result fs -> (
        match Float.Set.get_singleton fs with
        | None -> try_canonical_simple ()
        | Some f -> Lift (Boxed_float f)))
    | Value (Ok (Boxed_int32 (ty_naked_int32, _alloc_mode))) -> (
      match Provers.meet_naked_int32s env ty_naked_int32 with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result ns -> (
        match Int32.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int32 n)))
    | Value (Ok (Boxed_int64 (ty_naked_int64, _alloc_mode))) -> (
      match Provers.meet_naked_int64s env ty_naked_int64 with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result ns -> (
        match Int64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int64 n)))
    | Value (Ok (Boxed_nativeint (ty_naked_nativeint, _alloc_mode))) -> (
      match Provers.meet_naked_nativeints env ty_naked_nativeint with
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid
      | Known_result ns -> (
        match Targetint_32_64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_nativeint n)))
    | Value (Ok (Array { length; element_kind = _ })) -> (
      match Provers.meet_equals_single_tagged_immediate env length with
      | Known_result length ->
        if Targetint_31_63.equal length Targetint_31_63.zero
        then Lift Empty_array
        else try_canonical_simple ()
      | Need_meet -> try_canonical_simple ()
      | Invalid -> Invalid)
    | Value Bottom
    | Naked_immediate Bottom
    | Naked_float Bottom
    | Naked_int32 Bottom
    | Naked_int64 Bottom
    | Naked_nativeint Bottom
    | Rec_info Bottom
    | Region Bottom ->
      Invalid
    | Value Unknown
    | Value (Ok (String _))
    | Naked_immediate Unknown
    | Naked_float Unknown
    | Naked_int32 Unknown
    | Naked_int64 Unknown
    | Naked_nativeint Unknown
    | Rec_info Unknown
    | Region (Unknown | Ok _)
    | Rec_info (Ok _) ->
      try_canonical_simple ())
