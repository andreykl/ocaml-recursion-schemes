module StringSet = Set.Make (String)
open Ppxlib

let str_type_decl ~ctxt (rec_flag, tdecls) =
  let loc_code : location = Expansion_context.Deriver.derived_item_loc ctxt in
  let loc = { loc_code with loc_ghost = true } in
  if rec_flag = Nonrecursive then
    Location.raise_errorf ~loc
      "base_functor can be derived for recursive data types only"
  else
    let open Ast_builder.Default in
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

        let letters =
          let ca = Char.code 'a' in
          List.init 26 (fun i -> Char.chr (ca + i))
        in
        let rec fresh_name letters =
          match letters with
          | c :: rest ->
              let name = String.make 1 c in
              if not (StringSet.mem name all_names) then name
              else fresh_name rest
          | [] ->
              let rec find_with_number n =
                let name = "a" ^ string_of_int n in
                if not (StringSet.mem name all_names) then name
                else find_with_number (n + 1)
              in
              find_with_number 0
        in

        let param_name = fresh_name letters in

        let map_type (ct : core_type) : core_type =
          let ct = { ct with ptyp_loc = loc } in
          match ct.ptyp_desc with
          | Ptyp_constr ({ txt = Lident tname'; _ }, _) ->
              if tname = tname' then { ct with ptyp_desc = Ptyp_var param_name }
              else ct
          | _ -> ct
        in

        let map_constructor (ctor_decl : constructor_declaration) :
            constructor_declaration =
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
          (ptyp_var ~loc param_name, (NoVariance, NoInjectivity)) :: params
        in
        let ctors = List.map map_constructor ctors in
        let base_functor_decl : type_declaration =
          type_declaration ~loc
            ~name:(Located.mk ~loc @@ tname ^ "_bf")
            ~params ~cstrs:[] ~kind:(Ptype_variant ctors) ~private_:Public
            ~manifest:None
        in
        [ pstr_type ~loc Nonrecursive [ base_functor_decl ] ]
    | _ ->
        Location.raise_errorf ~loc
          "base_functor can be derived for variant types only"

let () =
  Deriving.add "base_functor"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore
