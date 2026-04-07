module StringSet = Set.Make (String)

module State = Monad.State.Make (struct
  type state = StringSet.t
end)

open Ppxlib

let letters =
  let ca = Char.code 'a' in
  List.init 26 (fun i -> Char.chr @@ (ca + i))

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

(* TODO: we suppose here pcd_res = None *)
let build_case ~loc ({ pcd_name; pcd_args; _ } : constructor_declaration)
    ~(transform_var : string -> expression -> expression) ~map_pat_ident
    ~map_exp_ident : case =
  let open Ast_builder.Make (struct
    let loc = loc
  end) in
  let open State in
  let open State.Syntax in
  let fresh : string State.t =
    let* s = get in
    let v, s = fresh_name letters s in
    let* () = put s in
    return v
  in

  let rec process_type : core_type -> (pattern * expression) State.t = function
    | { ptyp_desc = Ptyp_var lbl; _ } ->
        let pat = ppat_var @@ Located.mk lbl in
        let ident = pexp_ident @@ Located.lident lbl in
        let arg = transform_var lbl ident in
        return (pat, arg)
    | { ptyp_desc = Ptyp_tuple typs; _ } ->
        let* pes = mapM process_type typs in
        let patterns, expressions = List.split pes in
        return (ppat_tuple patterns, pexp_tuple expressions)
    | { ptyp_desc = Ptyp_constr (_, []); _ } ->
        (* concrete type like int, string -- no type parameters,
                  just pass through *)
        let* name = fresh in
        let pat = ppat_var @@ Located.mk name in
        let expr = pexp_ident @@ Located.lident name in
        return (pat, expr)
    | { ptyp_desc = Ptyp_constr (lidnt, args); _ } ->
        (* TODO: parametrized types inside constructors (e.g. 'a list)
                  are not supported. General solution: require user to provide
                  a map function via attribute, e.g.
                  | Foo of (int list [@map List.map]) *)
        let* pes = mapM process_type args in
        let patterns, expressions = List.split pes in
        ( ppat_construct lidnt @@ ppat_tuple_opt patterns,
          pexp_construct lidnt @@ pexp_tuple_opt expressions )
        |> return
    | _ ->
        (* TODO: handle other types (e.g. Ptyp_arrow, Ptyp_alias);
                  for now generate a fresh variable and pass through *)
        let* name = fresh in
        return (ppat_var @@ Located.mk name, pexp_ident @@ Located.lident name)
  in
  let pat_idnt = Located.map_lident @@ map_pat_ident pcd_name in
  let exp_idnt = Located.map_lident @@ map_exp_ident pcd_name in
  let pato, expro =
    match pcd_args with
    | Pcstr_tuple args ->
        let patterns, expressions =
          let pes, _ =
            State.runState (State.mapM process_type args) StringSet.empty
          in
          List.split pes
        in
        (ppat_tuple_opt patterns, pexp_tuple_opt expressions)
    | Pcstr_record _labels ->
        (* TODO: support records *)
        (None, None)
  in
  case ~guard:None
    ~lhs:(ppat_construct pat_idnt pato)
    ~rhs:(pexp_construct exp_idnt expro)

let map_constructor ~elem_sig ~param_name ~param_names ~tname ~loc
    (ctor_decl : constructor_declaration) : constructor_declaration =
  let open Ast_builder.Make (struct
    let loc = loc
  end) in
  (* TODO: nested recursive types (e.g. 'a myt list, 'a myt myt) are not supported.
             For the top-level recursive position (e.g. 'a myt) we replace with param_name.
             For nested positions, a map function is required.
             General solution: allow user to specify map functions via deriver argument:
               [@@deriving recursion_schemes { maps = [(list, List.map); (myt, map)] }]
             Naming convention (following ppx_deriving): map for type t, map_foo for type foo.
             Builtin types (list, option, etc.) use their standard map functions automatically.
             The maps argument overrides defaults when needed.
             Until then, raise an error when a recursive type appears in a nested position.
             Note: Elem provides types (objects), maps provides functions (morphisms) —
             this looks like a functor in the categorical sense. Worth exploring. *)

  (* map_type transforms types inside constructors for the Base functor:
             - 'a (type param) -> Elem.a
             - tname (recursive position, top-level) -> 'b (param_name)
             - tname (recursive position, nested) -> error

             ~nested tracks whether we are inside the arguments of another
             type constructor. At the top level (nested=false), a direct
             recursive reference like 'a myt is fine — it becomes 'b.
             Inside args of another constructor (nested=true), a recursive
             reference means we'd need a map function (e.g. List.map for
             'a myt list, or map for 'a myt myt) which we can't generate yet.

             Examples:
               'a myt       -> nested=false, replaced with 'b. OK.
               'a myt list  -> list ≠ myt, recurse into args with nested=true,
                               find myt with nested=true -> error.
               'a myt myt   -> outer myt matches, nested=false, but we check
                               args with nested=true, find inner myt -> error.
               'a myt * int -> tuple: recurse with same nested, find myt at
                               top level -> OK (replaced with 'b). *)
  let rec map_type ~nested (ct : core_type) : core_type =
    let ct = { ct with ptyp_loc = loc } in
    match ct.ptyp_desc with
    | Ptyp_var name when StringSet.mem name param_names ->
        ptyp_constr (Located.lident @@ elem_sig ^ "." ^ name) []
    | Ptyp_var _ ->
        ct (* should not happen since all names are in param_names *)
    | Ptyp_constr ({ txt = Lident tname'; _ }, args) when tname = tname' ->
        if nested then
          Location.raise_errorf ~loc
            "recursion_schemes: nested recursive type '%s' is not yet \
             supported (e.g. 'a %s %s, 'a %s list %s)."
            tname tname tname tname tname
        else begin
          (* Validate that args don't contain recursive references —
                    we're replacing the whole thing with 'b, so nested tname
                    inside args would be silently lost. *)
          ignore (List.map (map_type ~nested:true) args);
          { ct with ptyp_desc = Ptyp_var param_name }
        end
    | Ptyp_constr (lidnt, args) ->
        let args = List.map (map_type ~nested:true) args in
        { ct with ptyp_desc = Ptyp_constr (lidnt, args) }
    | Ptyp_tuple typs ->
        let typs = List.map (map_type ~nested) typs in
        { ct with ptyp_desc = Ptyp_tuple typs }
    | _ -> ct
  in
  let args =
    match ctor_decl.pcd_args with
    | Pcstr_tuple tuples ->
        let tuples = List.map (map_type ~nested:false) tuples in
        Pcstr_tuple tuples
    | Pcstr_record labels ->
        let map_label_decl ld =
          {
            ld with
            pld_loc = loc;
            pld_type = map_type ~nested:false ld.pld_type;
            pld_attributes =
              List.map (fun a -> { a with attr_loc = loc }) ld.pld_attributes;
          }
        in
        let labels = List.map map_label_decl labels in
        Pcstr_record labels
  in
  {
    ctor_decl with
    pcd_args = args;
    pcd_attributes =
      List.map (fun a -> { a with attr_loc = loc }) ctor_decl.pcd_attributes;
    pcd_loc = loc;
  }

let collect_names_in_type typ =
  let rec collect acc = function
    | { ptyp_desc = Ptyp_var name; _ } -> StringSet.add name acc
    | { ptyp_desc = Ptyp_constr (_, args); _ } ->
        List.fold_left collect acc args
    | { ptyp_desc = Ptyp_tuple types; _ } -> List.fold_left collect acc types
    | { ptyp_desc = Ptyp_arrow (_, t1, t2); _ } -> collect (collect acc t1) t2
    | { ptyp_desc = Ptyp_poly (vars, typ); _ } ->
        let acc =
          List.fold_left (fun acc var -> StringSet.add var.txt acc) acc vars
        in
        collect acc typ
    | _ -> acc
  in
  collect StringSet.empty typ

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
        let ( proj_fun,
              emb_fun,
              proj_mod,
              emb_mod,
              base_mod,
              std_typ_name,
              rs_mod,
              elem_sig ) =
          ("project", "embed", "Project", "Embed", "Base", "t", "RS", "Elem")
        in
        let rs_mod =
          if tname = std_typ_name then rs_mod
          else rs_mod ^ String.capitalize_ascii tname
        in
        let make_rs_mod = "Make_" ^ rs_mod in
        let incl =
          [%str include Recursion_schemes.Make (Base) (Project) (Embed)]
        in

        let param_names =
          List.fold_left
            (fun acc (param, _) ->
              match param with
              | { ptyp_desc = Ptyp_var name; _ } -> StringSet.add name acc
              | _ -> (* type parameters are always variables *) acc)
            StringSet.empty params
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

        let params =
          [ (ptyp_var param_name, (NoVariance, NoInjectivity)) ]
          (* :: params all other params moved to Elem.* *)
        in
        let ctors =
          List.map
            (map_constructor ~elem_sig ~loc ~tname ~param_name ~param_names)
            ctors
        in
        (* In Project, constructor labels are qualified in expressions (Base.Ctor);
           in Embed -- in patterns. So, map_pat_ident and map_exp_ident are swapped
           between Project and Embed. *)
        let make_projection_modules ~map_pat_ident ~map_exp_ident ~module_name
            ~fun_name =
          let transform_var _ = Fun.id in
          let cases =
            List.map
              (build_case ~loc ~transform_var ~map_pat_ident ~map_exp_ident)
              ctors
          in
          let the_fun =
            pexp_function [] None (Pfunction_cases (cases, loc, []))
          in
          let params = List.map (fun name -> ptyp_constr (Located.lident @@ elem_sig ^ "." ^ name) [])
                         (StringSet.to_list param_names) in
          let t_decl =
            type_declaration ~name:(Located.mk "t") ~params:[] ~cstrs:[]
              ~private_:Public
              ~manifest:(Some (ptyp_constr (Located.lident tname) params))
              ~kind:Ptype_abstract
          in
          let body =
            pmod_structure
              [
                pstr_module
                  (module_binding
                     ~name:(Located.mk @@ Some base_mod)
                     ~expr:(pmod_ident @@ Located.lident base_mod));
                pstr_type Nonrecursive [ t_decl ];
                pstr_value Nonrecursive
                  [
                    value_binding
                      ~pat:(ppat_var @@ Located.mk fun_name)
                      ~expr:the_fun;
                  ];
              ]
          in
          pstr_module
            (module_binding ~name:(Located.mk @@ Some module_name) ~expr:body)
        in
        let base_module =
          let base_functor_decl : type_declaration =
            type_declaration ~name:(Located.mk "t") ~params ~cstrs:[]
              ~kind:(Ptype_variant ctors) ~private_:Public ~manifest:None
          in
          let transform_var lbl ident =
            if lbl = param_name then
              pexp_apply (pexp_ident @@ Located.lident "f") [ (Nolabel, ident) ]
            else ident
          in
          let cases =
            List.map
              (build_case ~loc ~transform_var ~map_pat_ident:Fun.id
                 ~map_exp_ident:Fun.id)
              ctors
          in
          let map_function =
            pexp_function
              [ pparam_val Nolabel None (ppat_var @@ Located.mk "f") ]
              None
              (Pfunction_cases (cases, loc, []))
          in
          [%str
            module Base = struct
              [%%i pstr_type Nonrecursive [ base_functor_decl ]]

              let map = [%e map_function]
            end]
          (*          let body =
            pmod_structure
              [
                pstr_type Nonrecursive [ base_functor_decl ];
                pstr_value Nonrecursive
                  [
                    value_binding
                      ~pat:(ppat_var @@ Located.mk "map")
                      ~expr:map_function;
                  ];
              ]
          in
          pstr_module
            (module_binding ~name:(Located.mk @@ Some base_mod) ~expr:body)
 *)
        in
        let project_module =
          make_projection_modules ~map_pat_ident:Fun.id
            ~map_exp_ident:(Located.map (fun l -> base_mod ^ "." ^ l))
            ~module_name:proj_mod ~fun_name:proj_fun
        in

        let embed_module =
          make_projection_modules ~map_exp_ident:Fun.id
            ~map_pat_ident:(Located.map (fun l -> base_mod ^ "." ^ l))
            ~module_name:emb_mod ~fun_name:emb_fun
        in
        let elem_type_decls =
          List.map
            (fun name ->
              type_declaration ~name:(Located.mk name) ~params:[] ~cstrs:[]
                ~private_:Public ~manifest:None ~kind:Ptype_abstract)
            (StringSet.to_list param_names)
        in
        let module_rs =
          let body = 
          pmod_structure @@
              base_module @ [
              project_module ;
              embed_module  ] @
              incl
          in
          pstr_module
            (module_binding ~name:(Located.mk @@ Some rs_mod) ~expr:body)

        in
        let module_make_rs =
          pstr_module
            (module_binding
               ~name:(Located.mk @@ Some make_rs_mod)
               ~expr:
                 (pmod_functor
                    (Named
                       ( Located.mk @@ Some elem_sig,
                         pmty_signature
                           [ psig_type Nonrecursive elem_type_decls ] ))
                    (pmod_structure @@ base_module
                    @ [ project_module; embed_module ]
                    @ incl)))
        in
        if StringSet.is_empty param_names then [ module_rs ] else [ module_make_rs ]
    | _ ->
        Location.raise_errorf ~loc
          "recursion_schemes can be derived for variant types only"

let () =
  Deriving.add "recursion_schemes"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore
