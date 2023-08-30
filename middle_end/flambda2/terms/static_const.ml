(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let fprintf = Format.fprintf

type t =
  | Set_of_closures of Set_of_closures.t
  | Block of Tag.Scannable.t * Mutability.t * Field_of_static_block.t list
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Boxed_vec128 of Vector_types.Vec128.Bit_pattern.t Or_variable.t
  | Immutable_float_block of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_value_array of Field_of_static_block.t list
  | Empty_array
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

let set_of_closures set = Set_of_closures set

let block tag mutability fields = Block (tag, mutability, fields)

let boxed_float or_var = Boxed_float or_var

let boxed_int32 or_var = Boxed_int32 or_var

let boxed_int64 or_var = Boxed_int64 or_var

let boxed_nativeint or_var = Boxed_nativeint or_var

let boxed_vec128 or_var = Boxed_vec128 or_var

let immutable_float_block fields = Immutable_float_block fields

let immutable_float_array fields =
  match fields with [] -> Empty_array | _ :: _ -> Immutable_float_array fields

let immutable_value_array fields =
  match fields with [] -> Empty_array | _ :: _ -> Immutable_value_array fields

let empty_array = Empty_array

let mutable_string ~initial_value = Mutable_string { initial_value }

let immutable_string str = Immutable_string str

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Set_of_closures set ->
    fprintf ppf "@[<hov 1>(%tSet_of_closures%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      Set_of_closures.print set
  | Block (tag, mut, fields) ->
    fprintf ppf "@[<hov 1>(%t%sblock%t@ (tag %a)@ (%a))@]"
      Flambda_colours.static_part
      (match mut with
        | Immutable -> "Immutable_"
        | Immutable_unique -> "Unique_"
        | Mutable -> "Mutable_")
      Flambda_colours.pop
      Tag.Scannable.print tag
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Field_of_static_block.print) fields
  | Boxed_float or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_float%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Numeric_types.Float_by_bit_pattern.print) or_var
  | Boxed_int32 or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_int32%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Numeric_types.Int32.print) or_var
  | Boxed_int64 or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_int64%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Numeric_types.Int64.print) or_var
  | Boxed_nativeint or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_nativeint%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Targetint_32_64.print) or_var
  | Boxed_vec128 (or_var) ->
    fprintf ppf "@[<hov 1>(%tBoxed_vec128%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Vector_types.Vec128.Bit_pattern.print) or_var
  | Immutable_float_block fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_float_block%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Numeric_types.Float_by_bit_pattern.print))
      fields
  | Immutable_float_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_float_array%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Numeric_types.Float_by_bit_pattern.print))
      fields
  | Immutable_value_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_value_array%t@ (%a))@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Field_of_static_block.print) fields
  | Empty_array ->
    fprintf ppf "%tEmpty_array%t"
      Flambda_colours.static_part
      Flambda_colours.pop
  | Mutable_string { initial_value = s; } ->
    fprintf ppf "@[<hov 1>(%tMutable_string%t@ %S)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      s
  | Immutable_string s ->
    fprintf ppf "@[<hov 1>(%tImmutable_string%t@ %S)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      s

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let compare t1 t2 =
    match t1, t2 with
    | Set_of_closures set1, Set_of_closures set2 ->
      Set_of_closures.compare set1 set2
    | Block (tag1, mut1, fields1), Block (tag2, mut2, fields2) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else
        let c = Mutability.compare mut1 mut2 in
        if c <> 0
        then c
        else
          Misc.Stdlib.List.compare Field_of_static_block.compare fields1 fields2
    | Boxed_float or_var1, Boxed_float or_var2 ->
      Or_variable.compare Numeric_types.Float_by_bit_pattern.compare or_var1
        or_var2
    | Boxed_int32 or_var1, Boxed_int32 or_var2 ->
      Or_variable.compare Numeric_types.Int32.compare or_var1 or_var2
    | Boxed_int64 or_var1, Boxed_int64 or_var2 ->
      Or_variable.compare Numeric_types.Int64.compare or_var1 or_var2
    | Boxed_nativeint or_var1, Boxed_nativeint or_var2 ->
      Or_variable.compare Targetint_32_64.compare or_var1 or_var2
    | Boxed_vec128 or_var1, Boxed_vec128 or_var2 ->
      Or_variable.compare Vector_types.Vec128.Bit_pattern.compare or_var1
        or_var2
    | Immutable_float_block fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numeric_types.Float_by_bit_pattern.compare)
        fields1 fields2
    | Immutable_float_array fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numeric_types.Float_by_bit_pattern.compare)
        fields1 fields2
    | Immutable_value_array fields1, Immutable_value_array fields2 ->
      Misc.Stdlib.List.compare Field_of_static_block.compare fields1 fields2
    | Empty_array, Empty_array -> 0
    | ( Mutable_string { initial_value = s1 },
        Mutable_string { initial_value = s2 } )
    | Immutable_string s1, Immutable_string s2 ->
      String.compare s1 s2
    | Block _, _ -> -1
    | _, Block _ -> 1
    | Set_of_closures _, _ -> -1
    | _, Set_of_closures _ -> 1
    | Boxed_float _, _ -> -1
    | _, Boxed_float _ -> 1
    | Boxed_int32 _, _ -> -1
    | _, Boxed_int32 _ -> 1
    | Boxed_int64 _, _ -> -1
    | _, Boxed_int64 _ -> 1
    | Boxed_nativeint _, _ -> -1
    | _, Boxed_nativeint _ -> 1
    | Boxed_vec128 _, _ -> -1
    | _, Boxed_vec128 _ -> 1
    | Immutable_float_block _, _ -> -1
    | _, Immutable_float_block _ -> 1
    | Immutable_float_array _, _ -> -1
    | _, Immutable_float_array _ -> 1
    | Immutable_value_array _, _ -> -1
    | _, Immutable_value_array _ -> 1
    | Empty_array, _ -> -1
    | _, Empty_array -> 1
    | Mutable_string _, _ -> -1
    | Immutable_string _, Mutable_string _ -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let hash _t = Misc.fatal_error "Not yet implemented"
end)

let free_names_of_fields fields =
  List.fold_left
    (fun fvs field ->
      Name_occurrences.union fvs (Field_of_static_block.free_names field))
    Name_occurrences.empty fields

let free_names t =
  match t with
  | Set_of_closures set -> Set_of_closures.free_names set
  | Block (_tag, _mut, fields) -> free_names_of_fields fields
  | Boxed_float or_var -> Or_variable.free_names or_var
  | Boxed_int32 or_var -> Or_variable.free_names or_var
  | Boxed_int64 or_var -> Or_variable.free_names or_var
  | Boxed_nativeint or_var -> Or_variable.free_names or_var
  | Boxed_vec128 or_var -> Or_variable.free_names or_var
  | Mutable_string { initial_value = _ } | Immutable_string _ | Empty_array ->
    Name_occurrences.empty
  | Immutable_float_block fields | Immutable_float_array fields ->
    List.fold_left
      (fun fns (field : _ Or_variable.t) ->
        match field with
        | Var (v, _dbg) -> Name_occurrences.add_variable fns v Name_mode.normal
        | Const _ -> fns)
      Name_occurrences.empty fields
  | Immutable_value_array fields -> free_names_of_fields fields

let apply_renaming t renaming =
  if Renaming.is_identity renaming
  then t
  else
    match t with
    | Set_of_closures set ->
      let set' = Set_of_closures.apply_renaming set renaming in
      if set == set' then t else Set_of_closures set'
    | Block (tag, mut, fields) ->
      let fields' =
        Misc.Stdlib.List.map_sharing
          (fun field -> Field_of_static_block.apply_renaming field renaming)
          fields
      in
      if fields' == fields then t else Block (tag, mut, fields')
    | Boxed_float or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_float or_var'
    | Boxed_int32 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_int32 or_var'
    | Boxed_int64 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_int64 or_var'
    | Boxed_nativeint or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_nativeint or_var'
    | Boxed_vec128 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_vec128 or_var'
    | Mutable_string { initial_value = _ } | Immutable_string _ -> t
    | Immutable_float_block fields ->
      let fields' =
        Misc.Stdlib.List.map_sharing
          (fun (field : _ Or_variable.t) : _ Or_variable.t ->
            match field with
            | Var (v, dbg) ->
              let v' = Renaming.apply_variable renaming v in
              if v == v' then field else Var (v', dbg)
            | Const _ -> field)
          fields
      in
      if fields' == fields then t else Immutable_float_block fields'
    | Immutable_float_array fields ->
      let fields' =
        Misc.Stdlib.List.map_sharing
          (fun (field : _ Or_variable.t) : _ Or_variable.t ->
            match field with
            | Var (v, dbg) ->
              let v' = Renaming.apply_variable renaming v in
              if v == v' then field else Var (v', dbg)
            | Const _ -> field)
          fields
      in
      if fields' == fields then t else Immutable_float_array fields'
    | Immutable_value_array fields ->
      let fields' =
        Misc.Stdlib.List.map_sharing
          (fun field -> Field_of_static_block.apply_renaming field renaming)
          fields
      in
      if fields' == fields then t else Immutable_value_array fields'
    | Empty_array -> Empty_array

let ids_for_export_fields fields =
  List.fold_left
    (fun ids field ->
      Ids_for_export.union ids (Field_of_static_block.ids_for_export field))
    Ids_for_export.empty fields

let ids_for_export t =
  match t with
  | Set_of_closures set -> Set_of_closures.ids_for_export set
  | Block (_tag, _mut, fields) -> ids_for_export_fields fields
  | Boxed_float (Var (var, _dbg))
  | Boxed_int32 (Var (var, _dbg))
  | Boxed_int64 (Var (var, _dbg))
  | Boxed_nativeint (Var (var, _dbg))
  | Boxed_vec128 (Var (var, _dbg)) ->
    Ids_for_export.add_variable Ids_for_export.empty var
  | Boxed_float (Const _)
  | Boxed_int32 (Const _)
  | Boxed_int64 (Const _)
  | Boxed_nativeint (Const _)
  | Boxed_vec128 (Const _)
  | Mutable_string { initial_value = _ }
  | Immutable_string _ ->
    Ids_for_export.empty
  | Immutable_float_block fields ->
    List.fold_left
      (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var (var, _dbg) -> Ids_for_export.add_variable ids var
        | Const _ -> ids)
      Ids_for_export.empty fields
  | Immutable_float_array fields ->
    List.fold_left
      (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var (var, _dbg) -> Ids_for_export.add_variable ids var
        | Const _ -> ids)
      Ids_for_export.empty fields
  | Immutable_value_array fields -> ids_for_export_fields fields
  | Empty_array -> Ids_for_export.empty

let is_block t =
  match t with
  | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
  | Boxed_vec128 _ | Immutable_float_block _ | Immutable_float_array _
  | Immutable_string _ | Mutable_string _ | Empty_array
  | Immutable_value_array _ ->
    true
  | Set_of_closures _ -> false

let is_set_of_closures t =
  match t with
  | Set_of_closures _ -> true
  | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
  | Boxed_vec128 _ | Immutable_float_block _ | Immutable_float_array _
  | Immutable_string _ | Mutable_string _ | Empty_array
  | Immutable_value_array _ ->
    false

let is_fully_static t = free_names t |> Name_occurrences.no_variables

let can_share0 t =
  match t with
  | Block (_, Immutable, _)
  | Set_of_closures _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
  | Boxed_vec128 _ | Boxed_nativeint _ | Immutable_float_block _
  | Immutable_float_array _ | Immutable_string _ | Empty_array
  | Immutable_value_array _ ->
    true
  | Block (_, (Mutable | Immutable_unique), _) | Mutable_string _ -> false

let can_share t = can_share0 t && is_fully_static t

let must_be_set_of_closures t =
  match t with
  | Set_of_closures set -> set
  | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
  | Boxed_vec128 _ | Immutable_float_block _ | Immutable_float_array _
  | Empty_array | Immutable_value_array _ | Immutable_string _
  | Mutable_string _ ->
    Misc.fatal_errorf "Not a set of closures:@ %a" print t

let match_against_bound_static_pattern t (pat : Bound_static.Pattern.t)
    ~set_of_closures:set_of_closures_callback ~block_like:block_like_callback =
  match t, pat with
  | Set_of_closures set_of_closures, Set_of_closures closure_symbols ->
    let function_slots =
      Set_of_closures.function_decls set_of_closures
      |> Function_declarations.funs_in_order |> Function_slot.Lmap.keys
    in
    let function_slots' = Function_slot.Lmap.keys closure_symbols in
    let function_slots_match =
      (* Note that we check the order here. *)
      Misc.Stdlib.List.compare Function_slot.compare function_slots
        function_slots'
      = 0
    in
    if not function_slots_match
    then
      Misc.fatal_errorf "Mismatch on declared function slots:@ %a@ =@ %a"
        Bound_static.Pattern.print pat print t;
    set_of_closures_callback ~closure_symbols set_of_closures
  | ( ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_vec128 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Immutable_value_array _ | Empty_array | Immutable_string _
      | Mutable_string _ ),
      Block_like symbol ) ->
    block_like_callback symbol t
  | Set_of_closures _, (Block_like _ | Code _)
  | ( ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_vec128 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Immutable_value_array _ | Empty_array | Immutable_string _
      | Mutable_string _ ),
      (Set_of_closures _ | Code _) ) ->
    Misc.fatal_errorf "Mismatch on variety of [Static_const]:@ %a@ =@ %a"
      Bound_static.Pattern.print pat print t

let replace_field_of_static_block ~vars_to_replace field =
  let module F = Field_of_static_block in
  match (field : F.t) with
  | Symbol _ | Tagged_immediate _ -> field
  | Dynamically_computed (var, dbg) -> (
    match Variable.Map.find_opt var vars_to_replace with
    | None -> field
    | Some simple ->
      Simple.pattern_match' simple
        ~var:(fun var ~coercion:_ -> F.Dynamically_computed (var, dbg))
        ~symbol:(fun sym ~coercion:_ -> F.Symbol sym)
        ~const:(fun const ->
          match Reg_width_const.descr const with
          | Tagged_immediate imm -> F.Tagged_immediate imm
          | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ | Naked_vec128 _ ->
            Misc.fatal_errorf "Wrong constant for field of static block %a"
              Reg_width_const.print const))

let replace_or_variable ~vars_to_replace ~from_const or_var =
  match (or_var : _ Or_variable.t) with
  | Const _ -> or_var
  | Var (var, dbg) -> (
    match Variable.Map.find_opt var vars_to_replace with
    | None -> or_var
    | Some simple ->
      Simple.pattern_match' simple
        ~var:(fun var ~coercion:_ -> Or_variable.Var (var, dbg))
        ~symbol:(fun sym ~coercion:_ ->
          Misc.fatal_errorf "Unexpected symbol %a for replacing %a" Symbol.print
            sym Variable.print var)
        ~const:(fun const -> Or_variable.Const (from_const const)))

let from_float_const (const : Reg_width_const.t) =
  match Reg_width_const.descr const with
  | Naked_float f -> f
  | Naked_immediate _ | Tagged_immediate _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Naked_vec128 _ ->
    Misc.fatal_errorf "Wrong constant type for %a (expected float)"
      Reg_width_const.print const

let from_int32_const (const : Reg_width_const.t) =
  match Reg_width_const.descr const with
  | Naked_int32 i -> i
  | Naked_immediate _ | Tagged_immediate _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Naked_vec128 _ ->
    Misc.fatal_errorf "Wrong constant type for %a (expected int32)"
      Reg_width_const.print const

let from_int64_const (const : Reg_width_const.t) =
  match Reg_width_const.descr const with
  | Naked_int64 i -> i
  | Naked_immediate _ | Tagged_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Naked_vec128 _ ->
    Misc.fatal_errorf "Wrong constant type for %a (expected int64)"
      Reg_width_const.print const

let from_nativeint_const (const : Reg_width_const.t) =
  match Reg_width_const.descr const with
  | Naked_nativeint i -> i
  | Naked_immediate _ | Tagged_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ ->
    Misc.fatal_errorf "Wrong constant type for %a (expected nativeint)"
      Reg_width_const.print const

let from_vec128_const (const : Reg_width_const.t) =
  match Reg_width_const.descr const with
  | Naked_vec128 v -> v
  | Naked_immediate _ | Tagged_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Naked_nativeint _ ->
    Misc.fatal_errorf "Wrong constant type for %a (expected vec128)"
      Reg_width_const.print const

let replace_vars t ~vars_to_replace =
  match t with
  | Set_of_closures set ->
    let value_slots = Set_of_closures.value_slots set in
    let function_decls = Set_of_closures.function_decls set in
    let alloc_mode = Set_of_closures.alloc_mode set in
    let value_slots =
      Value_slot.Map.map
        (fun simple ->
          match Simple.must_be_var simple with
          | None -> simple
          | Some (var, coercion) -> (
            match Variable.Map.find_opt var vars_to_replace with
            | None -> simple
            | Some simple -> Simple.apply_coercion_exn simple coercion))
        value_slots
    in
    Set_of_closures
      (Set_of_closures.create ~value_slots alloc_mode function_decls)
  | Block (tag, mut, fields) ->
    let fields =
      List.map (replace_field_of_static_block ~vars_to_replace) fields
    in
    Block (tag, mut, fields)
  | Boxed_float contents ->
    let contents =
      replace_or_variable ~vars_to_replace ~from_const:from_float_const contents
    in
    Boxed_float contents
  | Boxed_int32 contents ->
    let contents =
      replace_or_variable ~vars_to_replace ~from_const:from_int32_const contents
    in
    Boxed_int32 contents
  | Boxed_int64 contents ->
    let contents =
      replace_or_variable ~vars_to_replace ~from_const:from_int64_const contents
    in
    Boxed_int64 contents
  | Boxed_nativeint contents ->
    let contents =
      replace_or_variable ~vars_to_replace ~from_const:from_nativeint_const
        contents
    in
    Boxed_nativeint contents
  | Boxed_vec128 contents ->
    let contents =
      replace_or_variable ~vars_to_replace ~from_const:from_vec128_const
        contents
    in
    Boxed_vec128 contents
  | Immutable_float_block fields ->
    let fields =
      List.map
        (replace_or_variable ~vars_to_replace ~from_const:from_float_const)
        fields
    in
    Immutable_float_block fields
  | Immutable_float_array fields ->
    let fields =
      List.map
        (replace_or_variable ~vars_to_replace ~from_const:from_float_const)
        fields
    in
    Immutable_float_array fields
  | Immutable_value_array fields ->
    let fields =
      List.map (replace_field_of_static_block ~vars_to_replace) fields
    in
    Immutable_value_array fields
  | Empty_array | Mutable_string _ | Immutable_string _ -> t
