open Ppxlib

let str_type_decl ~ctxt (_rec_flag, _tdecls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let open Ast_builder.Default in
  let x = "Z" in

  (* let st = [ pstr_type ~loc rec_flag tdecls ] in *)
  (* let _s = Format.asprintf "DERIVING input tdecls:\n%a\n%!" Pprintast.structure st in *)
  (* let () = Location.print Format.err_formatter loc in *)
  let ctor_x : constructor_declaration =
    constructor_declaration ~loc ~name:{ txt = x; loc } ~args:(Pcstr_tuple [])
      ~res:None
  in

  let ctor_s : constructor_declaration =
    constructor_declaration ~loc ~name:{ txt = "S"; loc }
      ~args:(Pcstr_tuple [ ptyp_constr ~loc { txt = Lident "nat"; loc } [] ])
      ~res:None
  in

  let nat_decl : type_declaration =
    type_declaration ~loc ~name:{ txt = "nat"; loc } ~params:[] ~cstrs:[]
      ~kind:(Ptype_variant [ ctor_x; ctor_s ])
      ~private_:Public ~manifest:None
  in

  [ pstr_type ~loc Recursive [ nat_decl ] ]

(*x
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [ [%stri type nat = S of nat | M ] ][@@subst let M : string = Z]
 ********)

let () =
  Deriving.add "base_functor"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore
