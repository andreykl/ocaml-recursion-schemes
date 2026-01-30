open Ppxlib

let str_type_decl ~ctxt (rec_flag, tdecls) =
  let loc : location = Expansion_context.Deriver.derived_item_loc ctxt in
  if rec_flag = Nonrecursive then
    Location.raise_errorf ~loc
      "base_functor can only be derived for recursive data types"
  else
    let open Ast_builder.Default in
    (match tdecls with
    | [ { ptype_name = { txt = tname; _ }; ptype_kind = Ptype_variant ctors; ptype_params = params; _ } ]
      ->
        (* let ctor_x : constructor_declaration = *)
        (*   constructor_declaration ~loc ~name:{ txt = "VVVVV"; loc } *)
        (*     ~args:(Pcstr_tuple []) ~res:None *)
        (* in *)

        (* let ctor_s : constructor_declaration = *)
        (*   constructor_declaration ~loc ~name:{ txt = "S"; loc } *)
        (*     ~args: *)
        (*       (Pcstr_tuple [ ptyp_constr ~loc { txt = Lident "nat"; loc } [] ]) *)
        (*     ~res:None *)
        (* in *)
       let map_type (ct : core_type) : core_type =
         let ct = {ct with ptyp_loc = loc} in
         match ct.ptyp_desc with
          | Ptyp_constr ({ txt = Lident tname'; _ }, []) ->
             if tname = tname' then { ct with ptyp_desc = Ptyp_var "a" }
             else ct
          | _ -> ct
        in

        let map_constructor (ctor_decl : constructor_declaration) :
            constructor_declaration =
          match ctor_decl.pcd_args with
          | Pcstr_tuple ts ->
              let ts = List.map map_type ts in
              { ctor_decl with pcd_args = Pcstr_tuple ts; pcd_loc = loc }
          | Pcstr_record _ ->
              failwith "base_functor for record types is not implemented"
        in

(*        let open Ppxlib.Expansion_helpers in
        let mngled = mangle ~fixpoint:(tname (Suffix "bf") tname in
 *)
        let params = params @ [(ptyp_var ~loc "a", (NoVariance, NoInjectivity)) ] in
        let ctors = List.map map_constructor ctors in
        let base_functor_decl : type_declaration =
          type_declaration ~loc
            ~name:(Located.mk ~loc @@ tname ^ "_bf")
            ~params:params
            ~cstrs:[] ~kind:(Ptype_variant ctors) ~private_:Public
            ~manifest:None
        in

        [ pstr_type ~loc Nonrecursive [ base_functor_decl ] ]
    | _ -> 
       Location.raise_errorf ~loc
         "base_functor can be derived for variant types only")
    [@warning "-27"]

let () =
  Deriving.add "base_functor"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore
