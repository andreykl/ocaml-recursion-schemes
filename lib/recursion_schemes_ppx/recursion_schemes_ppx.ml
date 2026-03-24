module StringSet = Set.Make (String)
open Ppxlib

let letters =
  let ca = Char.code 'a' in
  List.init 26 (fun i -> Char.chr (ca + i))

let fresh_name letters all_known_names =
  let rec fresh letters =
    match letters with
    | c :: rest ->
        let name = String.make 1 c in
        if not (StringSet.mem name all_known_names) then
          (name, StringSet.add name all_known_names)
        else fresh rest
    | [] ->
        let rec find_with_number n =
          let name = "a" ^ string_of_int n in
          if not (StringSet.mem name all_known_names) then
            (name, StringSet.add name all_known_names)
          else find_with_number (n + 1)
        in
        find_with_number 0
  in
  fresh letters

let str_type_decl ~ctxt (rec_flag, tdecls) =
  let loc_code : location = Expansion_context.Deriver.derived_item_loc ctxt in
  let loc = { loc_code with loc_ghost = true } in
  let open Ast_builder.Make (struct
    let loc = loc
  end) in
  if rec_flag = Nonrecursive then
    Location.raise_errorf ~loc
      "recursion_schemes can be derived for recursive data types only"
  else
    match tdecls with
    | [
     {
       ptype_name = { txt = tname; _ };
       ptype_kind = Ptype_variant ctors;
       ptype_params = params;
       _;
     };
    ] ->
        let param_names =
          List.fold_left
            (fun acc (param, _) ->
              match param with
              | { ptyp_desc = Ptyp_var name; _ } -> StringSet.add name acc
              | _ -> acc)
            StringSet.empty params
        in

        let collect_names_in_type typ =
          let rec collect acc = function
            | { ptyp_desc = Ptyp_var name; _ } -> StringSet.add name acc
            | { ptyp_desc = Ptyp_constr (_, args); _ } ->
                List.fold_left collect acc args
            | { ptyp_desc = Ptyp_tuple types; _ } ->
                List.fold_left collect acc types
            | { ptyp_desc = Ptyp_arrow (_, t1, t2); _ } ->
                collect (collect acc t1) t2
            | { ptyp_desc = Ptyp_poly (vars, typ); _ } ->
                let acc =
                  List.fold_left
                    (fun acc var -> StringSet.add var.txt acc)
                    acc vars
                in
                collect acc typ
            | _ -> acc
          in
          collect StringSet.empty typ
        in

        let names_in_constructors =
          List.fold_left
            (fun acc (ctor : constructor_declaration) ->
              match ctor.pcd_args with
              | Pcstr_tuple args ->
                  List.fold_left
                    (fun acc typ ->
                      StringSet.union acc @@ collect_names_in_type typ)
                    acc args
              | Pcstr_record labels ->
                  List.fold_left
                    (fun acc label ->
                      StringSet.union acc
                      @@ collect_names_in_type label.pld_type)
                    acc labels)
            StringSet.empty ctors
        in

        let all_names = StringSet.union param_names names_in_constructors in
        let param_name, _ = fresh_name letters all_names in

        let map_constructor (ctor_decl : constructor_declaration) :
            constructor_declaration =
          let map_type (ct : core_type) : core_type =
            let ct = { ct with ptyp_loc = loc } in
            match ct.ptyp_desc with
            | Ptyp_constr ({ txt = Lident tname'; _ }, _) ->
                if tname = tname' then
                  { ct with ptyp_desc = Ptyp_var param_name }
                else ct
            | _ -> ct
          in
          let args =
            match ctor_decl.pcd_args with
            | Pcstr_tuple tuples ->
                let tuples = List.map map_type tuples in
                Pcstr_tuple tuples
            | Pcstr_record labels ->
                let map_label_decl ld =
                  {
                    ld with
                    pld_loc = loc;
                    pld_type = map_type ld.pld_type;
                    pld_attributes =
                      List.map
                        (fun a -> { a with attr_loc = loc })
                        ld.pld_attributes;
                  }
                in
                let labels = List.map map_label_decl labels in
                Pcstr_record labels
          in
          {
            ctor_decl with
            pcd_args = args;
            pcd_attributes =
              List.map
                (fun a -> { a with attr_loc = loc })
                ctor_decl.pcd_attributes;
            pcd_loc = loc;
          }
        in

        let params =
          (ptyp_var param_name, (NoVariance, NoInjectivity)) :: params
        in
        let ctors = List.map map_constructor ctors in
        let base_functor_decl : type_declaration =
          type_declaration ~name:(Located.mk tname) ~params ~cstrs:[]
            ~kind:(Ptype_variant ctors) ~private_:Public ~manifest:None
        in
        let module_expr =
          let constructor_to_case
              ({ pcd_name; pcd_args; _ } : constructor_declaration) : case =
            let args_to_pat_expr (args : constructor_arguments) :
                pattern option * expression option =
              match args with
              | Pcstr_tuple args ->
                  let map_core_type_pe : core_type -> pattern * expression =
                    function
                    | { ptyp_desc = Ptyp_var lbl; _ } ->
                        let pat = ppat_var @@ Located.mk lbl in
                        let arg = pexp_ident (Located.lident lbl) in
                        let arg =
                          if lbl = param_name then
                            pexp_apply
                              (pexp_ident (Located.lident "f"))
                              [ (Nolabel, arg) ]
                          else arg
                        in
                        (pat, arg)
                    | { ptyp_desc = Ptyp_tuple _typs; _ } ->
                        (ppat_any, pexp_unreachable)
                    | { ptyp_desc = Ptyp_constr (_lidnt, _args); _ } ->
                        (ppat_any, pexp_unreachable)
                    | _ ->
                        (* TODO: support other types correctly *)
                        failwith "other types are not supported (yet?)"
                  in
                  (* let map_core_type_p *)
                  (*     ({ *)
                  (*        ptyp_desc = _; *)
                  (*        ptyp_loc = _; *)
                  (*        ptyp_loc_stack = _; *)
                  (*        ptyp_attributes = _; *)
                  (*      } : *)
                  (*       core_type) : pattern = *)
                  (*   ppat_any *)
                  (* in *)

                  let patterns, expressions =
                    List.split @@ List.map map_core_type_pe args
                  in

                  (*
                  List.fold_left
                    (fun acc typ ->
                      StringSet.union acc @@ collect_names_in_type typ)
                    acc args *)
                  (ppat_tuple_opt patterns, pexp_tuple_opt expressions)
              | Pcstr_record _labels -> (None, None)
              (* List.fold_left *)
              (*   (fun acc label -> *)
              (*     StringSet.union acc *)
              (*     @@ collect_names_in_type label.pld_type) *)
              (*   acc labels) *)

              (*              let ct = { ct with ptyp_loc = loc } in
              match ct.ptyp_desc with
              | Ptyp_constr ({ txt = Lident tname'; _ }, _) ->
                 if tname = tname' then { ct with ptyp_desc = Ptyp_var param_name }
                 else ct
              | _ -> ct
 *)
            in
            let idnt = Located.map_lident pcd_name in
            let pato, expro = args_to_pat_expr pcd_args in
            case ~guard:None ~lhs:(ppat_construct idnt pato)
              ~rhs:(pexp_construct idnt expro)
          in
          let cases = List.map constructor_to_case ctors in
          let exp_function =
            pexp_function
              [ pparam_val Nolabel None (ppat_var @@ Located.mk "f") ]
              None
              (Pfunction_cases (cases, loc, []))
          in
          let value_binding =
            value_binding ~pat:(ppat_var @@ Located.mk "map") ~expr:exp_function
          in
          pmod_structure
            [
              pstr_type Nonrecursive [ base_functor_decl ];
              pstr_value Nonrecursive [ value_binding ];
            ]
        in
        let module_binding =
          module_binding ~name:(Located.mk (Some "Base")) ~expr:module_expr
        in
        [ pstr_module module_binding ]
    (* [ pstr_type ~loc Nonrecursive [ base_functor_decl ] ] *)
    | _ ->
        Location.raise_errorf ~loc
          "recursion_schemes can be derived for variant types only"

let () =
  Deriving.add "recursion_schemes"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore
