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

(* CR mshinwell: enable warning fragile-match *)

[@@@ocaml.warning "-fragile-match"]

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module K = Flambda_kind
module TE = Typing_env
module TG = Type_grammar

let is_bottom = Expand_head.is_bottom

let expand_head env ty =
  Expand_head.expand_head env ty |> Expand_head.Expanded_type.descr_oub

type 'a meet_shortcut =
  | Known_result of 'a
  | Need_meet
  | Invalid

type 'a proof_of_property =
  | Proved of 'a
  | Unknown
  | Wrong_kind
(* CR: try to find real uses and remove the rest *)

type 'a generic_proof =
  | Proved of 'a
  | Unknown
  | Invalid
  | Wrong_kind

(* CR: Naming Either Sub-module or complete name *)
let as_meet_shortcut (p : _ generic_proof) : _ meet_shortcut =
  match p with
  | Proved x -> Known_result x
  | Unknown -> Need_meet
  (* CR: Consider fatal error for Wrong_kind *)
  | Invalid | Wrong_kind -> Invalid

let as_property (p : _ generic_proof) : _ proof_of_property =
  match p with
  | Proved x -> Proved x
  | Unknown | Invalid -> Unknown
  | Wrong_kind -> Wrong_kind

let prove_equals_to_simple_of_kind_value env t : Simple.t proof_of_property =
  let original_kind = TG.kind t in
  if not (K.equal original_kind K.value)
  then Wrong_kind
  else
    (* CR: add TE.get_alias_opt *)
    match TG.get_alias_exn t with
    | exception Not_found ->
      (* CR vlaviron: We could try to turn singleton types into constants here.
         I think there are already a few hacks to generate constant aliases
         instead of singleton types when possible, so we might never end up here
         with a singleton type in practice. *)
      Unknown
    | simple -> (
      match
        TE.get_canonical_simple_exn env simple ~min_name_mode:Name_mode.normal
      with
      | exception Not_found -> Unknown
      | simple -> Proved simple)

let prove_is_int_generic env t : bool generic_proof =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.blocks, blocks_imms.immediates with
    | Unknown, Unknown -> Unknown
    | Unknown, Known imms ->
      if is_bottom env imms then Proved false else Unknown
    | Known blocks, Unknown ->
      if TG.Row_like_for_blocks.is_bottom blocks then Proved true else Unknown
    | Known blocks, Known imms ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then if is_bottom env imms then Invalid else Proved true
      else if is_bottom env imms
      then Proved false
      else Unknown)
  | Value
      (Ok
        ( Mutable_block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
        | Boxed_nativeint _ | Closures _ | String _ | Array _ )) ->
    Proved false
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Wrong_kind

let prove_is_int env t = as_property (prove_is_int_generic env t)

(* Note: this function returns a generic proof because we want to propagate the
   Invalid cases to prove_naked_immediates_generic, but it's not suitable for
   implementing [meet_get_tag] because it doesn't ignore the immediates part of
   the variant. *)
(* CR: Switch to Tag.Scannable *)
let prove_get_tag_generic env t : Tag.Set.t generic_proof =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.immediates with
    | Unknown -> Unknown
    | Known imms -> (
      if not (is_bottom env imms)
      then Unknown
      else
        match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks -> (
          (* CR mshinwell: maybe [all_tags] should return the [Invalid] case
             directly? *)
          match TG.Row_like_for_blocks.all_tags blocks with
          | Unknown -> Unknown
          | Known tags -> if Tag.Set.is_empty tags then Invalid else Proved tags
          )))
  | Value
      (Ok (Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _))
    ->
    Unknown
  | Value (Ok (Mutable_block _)) -> Unknown
  | Value (Ok (Closures _)) -> Unknown
  | Value (Ok (String _)) -> Unknown
  | Value (Ok (Array _)) -> Unknown
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Wrong_kind

let prove_get_tag env t = as_property (prove_get_tag_generic env t)

(* CR: Specialise to meet, Wrong_kind -> fatal error *)
let prove_naked_immediates_generic env t : Targetint_31_63.Set.t generic_proof =
  match expand_head env t with
  | Naked_immediate (Ok (Naked_immediates is)) ->
    if Targetint_31_63.Set.is_empty is then Invalid else Proved is
  | Naked_immediate (Ok (Is_int scrutinee_ty)) -> (
    match prove_is_int_generic env scrutinee_ty with
    | Proved true ->
      Proved (Targetint_31_63.Set.singleton Targetint_31_63.bool_true)
    | Proved false ->
      Proved (Targetint_31_63.Set.singleton Targetint_31_63.bool_false)
    | Unknown -> Unknown
    | Invalid -> Invalid
    | Wrong_kind -> Wrong_kind)
  | Naked_immediate (Ok (Get_tag block_ty)) -> (
    match prove_get_tag_generic env block_ty with
    | Proved tags ->
      let is =
        Tag.Set.fold
          (fun tag is ->
            Targetint_31_63.Set.add (Tag.to_targetint_31_63 tag) is)
          tags Targetint_31_63.Set.empty
      in
      Proved is
    | Unknown -> Unknown
    | Invalid -> Invalid
    | Wrong_kind -> Wrong_kind)
  | Naked_immediate Unknown -> Unknown
  | Naked_immediate Bottom -> Invalid
  | Value _ -> Wrong_kind
  | Naked_float _ -> Wrong_kind
  | Naked_int32 _ -> Wrong_kind
  | Naked_int64 _ -> Wrong_kind
  | Naked_nativeint _ -> Wrong_kind
  | Rec_info _ -> Wrong_kind
  | Region _ -> Wrong_kind

let meet_naked_immediates env t =
  as_meet_shortcut (prove_naked_immediates_generic env t)

let prove_equals_tagged_immediates env t : _ proof_of_property =
  match expand_head env t with
  | Value (Ok (Variant { immediates; blocks; is_unique = _; alloc_mode = _ }))
    -> (
    match blocks with
    | Unknown -> Unknown
    | Known blocks ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then
        match immediates with
        | Unknown -> Unknown
        | Known imms -> (
          match prove_naked_immediates_generic env imms with
          | Proved imms -> Proved imms
          | Invalid -> Proved Targetint_31_63.Set.empty
          | Unknown -> Unknown
          | Wrong_kind -> Wrong_kind)
      else Unknown)
  | Value (Ok _ | Unknown | Bottom) -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Wrong_kind

let meet_equals_tagged_immediates env t : _ meet_shortcut =
  match expand_head env t with
  | Value
      (Ok (Variant { immediates; blocks = _; is_unique = _; alloc_mode = _ }))
    -> (
    match immediates with
    | Unknown -> Need_meet
    | Known imms -> meet_naked_immediates env imms)
  | Value
      (Ok
        ( Mutable_block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
        | Boxed_nativeint _ | Closures _ | String _ | Array _ )) ->
    Invalid
  | Value Unknown -> Need_meet
  | Value Bottom
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Invalid

let meet_equals_single_tagged_immediate env t : _ meet_shortcut =
  match meet_equals_tagged_immediates env t with
  | Known_result imms -> (
    match Targetint_31_63.Set.get_singleton imms with
    | Some imm -> Known_result imm
    | None -> Need_meet)
  | Need_meet -> Need_meet
  | Invalid -> Invalid

type _ meet_naked_number_kind =
  | Float : Float.Set.t meet_naked_number_kind
  | Int32 : Int32.Set.t meet_naked_number_kind
  | Int64 : Int64.Set.t meet_naked_number_kind
  | Nativeint : Targetint_32_64.Set.t meet_naked_number_kind

let[@inline] meet_naked_number (type a) (kind : a meet_naked_number_kind) env t
    : a meet_shortcut =
  let head_to_proof (head : _ Or_unknown_or_bottom.t) coercion ~is_empty :
      _ meet_shortcut =
    match head with
    | Ok set ->
      let set = coercion set in
      if is_empty set then Invalid else Known_result set
    | Unknown -> Need_meet
    | Bottom -> Invalid
  in
  let wrong_kind () =
    let kind_string =
      match kind with
      | Float -> "Naked_float"
      | Int32 -> "Naked_int32"
      | Int64 -> "Naked_int64"
      | Nativeint -> "Naked_nativeint"
    in
    Misc.fatal_errorf "Kind error: expected [%s]:@ %a" kind_string TG.print t
  in
  match expand_head env t with
  | Value _ -> wrong_kind ()
  | Naked_immediate _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()
  | Region _ -> wrong_kind ()
  | Naked_float fs -> (
    match kind with
    | Float ->
      head_to_proof fs
        (fun (fs : TG.head_of_kind_naked_float) -> (fs :> Float.Set.t))
        ~is_empty:Float.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_int32 is -> (
    match kind with
    | Int32 ->
      head_to_proof is
        (fun (is : TG.head_of_kind_naked_int32) -> (is :> Int32.Set.t))
        ~is_empty:Int32.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_int64 is -> (
    match kind with
    | Int64 ->
      head_to_proof is
        (fun (is : TG.head_of_kind_naked_int64) -> (is :> Int64.Set.t))
        ~is_empty:Int64.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_nativeint is -> (
    match kind with
    | Nativeint ->
      head_to_proof is
        (fun (is : TG.head_of_kind_naked_nativeint) ->
          (is :> Targetint_32_64.Set.t))
        ~is_empty:Targetint_32_64.Set.is_empty
    | _ -> wrong_kind ())

let meet_naked_floats = meet_naked_number Float

let meet_naked_int32s = meet_naked_number Int32

let meet_naked_int64s = meet_naked_number Int64

let meet_naked_nativeints = meet_naked_number Nativeint

type variant_like_proof =
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes : Targetint_31_63.t Tag.Scannable.Map.t
  }

let prove_variant_like_generic env t : variant_like_proof generic_proof =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.blocks with
    | Unknown -> Unknown
    | Known blocks -> (
      match TG.Row_like_for_blocks.all_tags_and_sizes blocks with
      | Unknown -> Unknown
      | Known non_const_ctors_with_sizes -> (
        let non_const_ctors_with_sizes =
          (* CR: we could ignore non-scannable tags for the meet_ version *)
          Tag.Map.fold
            (fun tag size (result : _ Or_unknown.t) : _ Or_unknown.t ->
              match result with
              | Unknown -> Unknown
              | Known result -> (
                match Tag.Scannable.of_tag tag with
                | None -> Unknown
                | Some tag -> Known (Tag.Scannable.Map.add tag size result)))
            non_const_ctors_with_sizes
            (Or_unknown.Known Tag.Scannable.Map.empty)
        in
        match non_const_ctors_with_sizes with
        | Unknown -> Unknown
        | Known non_const_ctors_with_sizes ->
          let const_ctors : _ Or_unknown.t =
            match blocks_imms.immediates with
            | Unknown -> Unknown
            | Known imms -> (
              match prove_naked_immediates_generic env imms with
              | Unknown -> Unknown
              | Invalid -> Known Targetint_31_63.Set.empty
              | Proved const_ctors -> Known const_ctors
              | Wrong_kind -> Misc.fatal_error "To remove")
          in
          Proved { const_ctors; non_const_ctors_with_sizes })))
  | Value (Ok (Mutable_block _)) -> Unknown
  | Value (Ok (Array _)) ->
    Unknown (* We could return Invalid in a strict mode *)
  | Value
      (Ok
        ( Closures _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
        | Boxed_nativeint _ | String _ )) ->
    Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ -> Wrong_kind
  | Naked_float _ -> Wrong_kind
  | Naked_int32 _ -> Wrong_kind
  | Naked_int64 _ -> Wrong_kind
  | Naked_nativeint _ -> Wrong_kind
  | Rec_info _ -> Wrong_kind
  | Region _ -> Wrong_kind

let meet_variant_like env t =
  as_meet_shortcut (prove_variant_like_generic env t)

let prove_variant_like env t = as_property (prove_variant_like_generic env t)

type boxed_or_tagged_number =
  | Boxed of Flambda_kind.Boxable_number.t
  | Tagged_immediate

(* CR: Remove fragile matchs and reuse this function *)
let prove_is_a_boxed_or_tagged_number env t :
    boxed_or_tagged_number proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value
      (Ok (Variant { blocks; immediates = _; is_unique = _; alloc_mode = _ }))
    -> (
    match blocks with
    | Unknown -> Unknown
    | Known blocks ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then Proved Tagged_immediate
      else Unknown)
  | Value (Ok (Boxed_float _)) -> Proved (Boxed Naked_float)
  | Value (Ok (Boxed_int32 _)) -> Proved (Boxed Naked_int32)
  | Value (Ok (Boxed_int64 _)) -> Proved (Boxed Naked_int64)
  | Value (Ok (Boxed_nativeint _)) -> Proved (Boxed Naked_nativeint)
  | Value _ -> Unknown
  | _ -> Wrong_kind

let prove_is_a_tagged_immediate env t : _ proof_of_property =
  match prove_is_a_boxed_or_tagged_number env t with
  | Proved Tagged_immediate -> Proved ()
  | Proved _ -> Unknown
  | Wrong_kind -> Wrong_kind
  | Unknown -> Unknown

let prove_is_a_boxed_float env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_float _)) -> Proved ()
  | Value _ -> Unknown
  | _ -> Wrong_kind

let prove_is_or_is_not_a_boxed_float env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value Bottom -> Unknown
  | Value (Ok (Boxed_float _)) -> Proved true
  | Value (Ok _) -> Proved false
  | _ -> Wrong_kind

let prove_is_a_boxed_int32 env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int32 _)) -> Proved ()
  | Value _ -> Unknown
  | _ -> Wrong_kind

let prove_is_a_boxed_int64 env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int64 _)) -> Proved ()
  | Value _ -> Unknown
  | _ -> Wrong_kind

let prove_is_a_boxed_nativeint env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_nativeint _)) -> Proved ()
  | Value _ -> Unknown
  | _ -> Wrong_kind

let prove_unique_tag_and_size0 env t :
    (Tag_and_size.t * TG.Product.Int_indexed.t * Alloc_mode.t Or_unknown.t)
    proof_of_property =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.immediates with
    | Unknown -> Unknown
    | Known immediates ->
      if is_bottom env immediates
      then
        match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks -> (
          match TG.Row_like_for_blocks.get_singleton blocks with
          | None -> Unknown
          | Some (tag_and_size, product) ->
            Proved (tag_and_size, product, blocks_imms.alloc_mode))
      else Unknown)
  | Value (Ok (Mutable_block _)) | Value (Ok _) | Value Unknown | Value Bottom
    ->
    Unknown
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Wrong_kind

let prove_unique_tag_and_size env t :
    (Tag.t * Targetint_31_63.t) proof_of_property =
  match prove_unique_tag_and_size0 env t with
  | Proved (tag_and_size, _, _) -> Proved tag_and_size
  | Unknown -> Unknown
  | Wrong_kind -> Wrong_kind

let prove_unique_fully_constructed_immutable_heap_block env t :
    _ proof_of_property =
  match prove_unique_tag_and_size0 env t with
  | Wrong_kind -> Wrong_kind
  | Unknown | Proved (_, _, (Unknown | Known Local)) -> Unknown
  | Proved (tag_and_size, product, Known Heap) -> (
    let result =
      List.fold_left
        (fun (result : _ proof_of_property) field_ty : _ proof_of_property ->
          match result with
          | Wrong_kind | Unknown -> result
          | Proved simples_rev -> (
            match TG.get_alias_exn field_ty with
            | exception Not_found -> Unknown
            | simple -> Proved (simple :: simples_rev)))
        (Proved [] : _ proof_of_property)
        (TG.Product.Int_indexed.components product)
    in
    match result with
    | Wrong_kind -> Wrong_kind
    | Unknown -> Unknown
    | Proved simples -> Proved (tag_and_size, List.rev simples))

type array_kind_compatibility =
  | Exact
  | Compatible
  | Incompatible

let prove_is_flat_float_array env t : bool proof_of_property =
  match expand_head env t with
  | Value (Unknown | Bottom) -> Unknown
  | Value (Ok (Array { element_kind = Unknown; _ })) -> Unknown
  | Value (Ok (Array { element_kind = Known element_kind; _ })) -> (
    match K.With_subkind.kind element_kind with
    | Value -> Proved false
    | Naked_number Naked_float -> Proved true
    | Naked_number
        (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint)
    | Region | Rec_info ->
      Misc.fatal_errorf "Wrong element kind for array: %a" K.With_subkind.print
        element_kind)
  | Value
      (Ok
        ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int32 _
        | Boxed_int64 _ | Boxed_nativeint _ | Closures _ | String _ )) ->
    Unknown
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Wrong_kind
(* Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t *)

let meet_is_array_with_element_kind env t ~element_kind : _ meet_shortcut =
  match expand_head env t with
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Value (Ok (Array { element_kind = Unknown; _ })) -> Need_meet
  | Value (Ok (Array { element_kind = Known element_kind'; _ })) ->
    if K.With_subkind.equal element_kind' element_kind
    then Known_result Exact
    else if K.With_subkind.compatible element_kind ~when_used_at:element_kind'
            || K.With_subkind.compatible element_kind'
                 ~when_used_at:element_kind
    then Known_result Compatible
    else Known_result Incompatible
  | Value (Ok (Variant _ | Mutable_block _)) ->
    (* CR vlaviron: This case is here to avoiding breaking code such as:
     * Array.get (Obj.magic (0, 0)) 1
     * If we decide that this code should segfault, we could return Invalid. *)
    Need_meet
  | Value
      (Ok
        ( Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
        | Closures _ | String _ )) ->
    Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

let prove_single_closures_entry_generic env t : _ generic_proof =
  match expand_head env t with
  | Value (Ok (Closures { by_function_slot; alloc_mode })) -> (
    match TG.Row_like_for_closures.get_singleton by_function_slot with
    | None -> Unknown
    | Some ((function_slot, set_of_closures_contents), closures_entry) -> (
      let function_slots =
        Set_of_closures_contents.closures set_of_closures_contents
      in
      assert (Function_slot.Set.mem function_slot function_slots);
      let function_type =
        TG.Closures_entry.find_function_type closures_entry function_slot
      in
      match function_type with
      | Bottom -> Invalid
      | Unknown -> Unknown
      | Ok function_type ->
        Proved (function_slot, alloc_mode, closures_entry, function_type)))
  | Value
      (Ok
        ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int32 _
        | Boxed_int64 _ | Boxed_nativeint _ | String _ | Array _ )) ->
    Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ -> Wrong_kind
  | Naked_float _ -> Wrong_kind
  | Naked_int32 _ -> Wrong_kind
  | Naked_int64 _ -> Wrong_kind
  | Naked_nativeint _ -> Wrong_kind
  | Rec_info _ -> Wrong_kind
  | Region _ -> Wrong_kind

let meet_single_closures_entry env t =
  as_meet_shortcut (prove_single_closures_entry_generic env t)

let prove_single_closures_entry env t =
  as_property (prove_single_closures_entry_generic env t)

let meet_strings env t : String_info.Set.t meet_shortcut =
  match expand_head env t with
  | Value (Ok (String strs)) -> Known_result strs
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

type tagging_proof_kind =
  | Prove
  | Check

let prove_tagging_of_simple_aux proof_kind env ~min_name_mode t :
    Simple.t generic_proof =
  match expand_head env t with
  | Value (Ok (Variant { immediates; blocks; is_unique = _; alloc_mode = _ }))
    -> (
    let prove_immediates () =
      match immediates with
      | Unknown -> Unknown
      | Known t -> (
        let from_alias =
          match
            TE.get_canonical_simple_exn env ~min_name_mode (TG.get_alias_exn t)
          with
          | simple -> Some simple
          | exception Not_found -> None
        in
        match from_alias with
        | Some simple -> Proved simple
        | None -> (
          match meet_naked_immediates env t with
          | Need_meet -> Unknown
          | Invalid -> Invalid
          | Known_result imms -> (
            match Targetint_31_63.Set.get_singleton imms with
            | Some imm ->
              Proved (Simple.const (Reg_width_const.naked_immediate imm))
            | None -> Unknown)))
    in
    match proof_kind, blocks with
    | Prove, Unknown -> Unknown
    | Prove, Known blocks ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then prove_immediates ()
      else Unknown
    | Check, _ -> prove_immediates ())
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Wrong_kind

let prove_tagging_of_simple env ~min_name_mode t =
  as_property (prove_tagging_of_simple_aux Prove env ~min_name_mode t)

let meet_tagging_of_simple env ~min_name_mode t =
  as_meet_shortcut (prove_tagging_of_simple_aux Check env ~min_name_mode t)

let[@inline always] meet_boxed_number_containing_simple
    ~contents_of_boxed_number env ~min_name_mode t : Simple.t meet_shortcut =
  match expand_head env t with
  | Value (Ok ty_value) -> (
    match contents_of_boxed_number ty_value with
    | None -> Invalid
    | Some ty -> (
      match
        TE.get_canonical_simple_exn env ~min_name_mode (TG.get_alias_exn ty)
      with
      | simple -> Known_result simple
      | exception Not_found -> Need_meet))
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

let meet_boxed_float_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_float (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Closures _ | String _ | Array _ ->
        None)

let meet_boxed_int32_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_int32 (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int64 _
      | Boxed_nativeint _ | Closures _ | String _ | Array _ ->
        None)

let meet_boxed_int64_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_int64 (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int32 _
      | Boxed_nativeint _ | Closures _ | String _ | Array _ ->
        None)

let meet_boxed_nativeint_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_nativeint (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int32 _
      | Boxed_int64 _ | Closures _ | String _ | Array _ ->
        None)

let[@inline] meet_block_field_simple_aux env ~min_name_mode t get_field :
    Simple.t meet_shortcut =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value
      (Ok (Variant { immediates = _; blocks; is_unique = _; alloc_mode = _ }))
    -> (
    match blocks with
    | Unknown -> Need_meet
    | Known blocks -> (
      if TG.Row_like_for_blocks.is_bottom blocks
      then Invalid
      else
        match (get_field blocks : _ Or_unknown_or_bottom.t) with
        | Bottom -> Invalid
        | Unknown -> Need_meet
        | Ok ty -> (
          match TG.get_alias_exn ty with
          | simple -> (
            match TE.get_canonical_simple_exn env ~min_name_mode simple with
            | simple -> Known_result simple
            | exception Not_found -> Need_meet)
          | exception Not_found -> Need_meet)))
  | Value (Ok (Mutable_block _)) -> Need_meet
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    wrong_kind ()

let meet_block_field_simple env ~min_name_mode t field_index =
  let[@inline] get blocks =
    TG.Row_like_for_blocks.get_field blocks field_index
  in
  (meet_block_field_simple_aux [@inlined]) env ~min_name_mode t get

let meet_variant_field_simple env ~min_name_mode t variant_tag field_index =
  let[@inline] get blocks =
    TG.Row_like_for_blocks.get_variant_field blocks variant_tag field_index
  in
  (meet_block_field_simple_aux [@inlined]) env ~min_name_mode t get

let meet_project_function_slot_simple env ~min_name_mode t function_slot :
    Simple.t meet_shortcut =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Closures { by_function_slot; alloc_mode = _ })) -> (
    match
      TG.Row_like_for_closures.get_closure by_function_slot function_slot
    with
    | Unknown -> Need_meet
    | Known ty -> (
      match TG.get_alias_exn ty with
      | simple -> (
        match TE.get_canonical_simple_exn env ~min_name_mode simple with
        | simple -> Known_result simple
        | exception Not_found -> Need_meet)
      | exception Not_found -> Need_meet))
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    wrong_kind ()

let meet_project_value_slot_simple env ~min_name_mode t env_var :
    Simple.t meet_shortcut =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Closures { by_function_slot; alloc_mode = _ })) -> (
    match TG.Row_like_for_closures.get_env_var by_function_slot env_var with
    | Unknown -> Need_meet
    | Known ty -> (
      match TG.get_alias_exn ty with
      | simple -> (
        match TE.get_canonical_simple_exn env ~min_name_mode simple with
        | simple -> Known_result simple
        | exception Not_found -> Need_meet)
      | exception Not_found -> Need_meet))
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    wrong_kind ()

let meet_rec_info env t : Rec_info_expr.t meet_shortcut =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Rec_info]:@ %a" TG.print t
  in
  match expand_head env t with
  | Rec_info (Ok rec_info_expr) -> Known_result rec_info_expr
  | Rec_info Unknown -> Need_meet
  | Rec_info Bottom -> Invalid
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Region _ ->
    wrong_kind ()

let prove_alloc_mode_of_boxed_number env t : Alloc_mode.t proof_of_property =
  match expand_head env t with
  | Value (Ok (Boxed_float (_, alloc_mode)))
  | Value (Ok (Boxed_int32 (_, alloc_mode)))
  | Value (Ok (Boxed_int64 (_, alloc_mode)))
  | Value (Ok (Boxed_nativeint (_, alloc_mode))) -> (
    match alloc_mode with
    | Unknown -> Unknown
    | Known alloc_mode -> Proved alloc_mode)
  | Value (Ok (Variant _ | Mutable_block _ | String _ | Array _ | Closures _))
  | Value (Unknown | Bottom) ->
    Unknown
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Wrong_kind

let never_holds_locally_allocated_values env var kind : _ proof_of_property =
  let t = TE.find env (Name.var var) (Some kind) in
  match expand_head env t with
  | Value (Ok (Boxed_float (_, alloc_mode)))
  | Value (Ok (Boxed_int32 (_, alloc_mode)))
  | Value (Ok (Boxed_int64 (_, alloc_mode)))
  | Value (Ok (Boxed_nativeint (_, alloc_mode)))
  | Value (Ok (Variant { alloc_mode; _ }))
  | Value (Ok (Mutable_block { alloc_mode }))
  | Value (Ok (Closures { alloc_mode; _ })) -> (
    match alloc_mode with
    | Known Heap -> Proved ()
    | Known Local | Unknown -> Unknown)
  | Value (Ok (Array _)) ->
    (* CR mshinwell: For this function it would now be useful to track the alloc
       mode on arrays. *)
    Unknown
  | Value (Ok (String _)) -> Proved ()
  | Value Unknown -> Unknown
  | Value Bottom -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ | Region _ ->
    Proved ()
