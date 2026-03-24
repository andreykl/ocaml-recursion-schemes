open Ppxlib

let str_type_decl ~ctxt (_rec_flag, tdecls) =
  let loc_code : location = Expansion_context.Deriver.derived_item_loc ctxt in
  let loc = { loc_code with loc_ghost = true } in
  let open Ast_builder.Default in
  match tdecls with
  | [ { ptype_name = { txt = tname; _ }; _ } ] ->
      let type_decl =
        type_declaration ~loc ~name:(Located.mk ~loc "t") ~params:[] ~cstrs:[]
          ~kind:Ptype_abstract ~private_:Public
          ~manifest:
            (Some (ptyp_constr ~loc (Located.mk ~loc (Lident tname)) []))
      in
      let module_expr =
        pmod_structure ~loc [ pstr_type ~loc Nonrecursive [ type_decl ] ]
      in
      let module_binding =
        module_binding ~loc
          ~name:(Located.mk ~loc (Some "Base"))
          ~expr:module_expr
      in
      [ pstr_module ~loc module_binding ]
  | _ ->
      Location.raise_errorf ~loc
        "base_module can be derived for a single type declaration only"

let () =
  Deriving.add "base_module"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore

(*

Вот вариант с metaquot:

```ocaml
open Ppxlib

let str_type_decl ~ctxt (_rec_flag, tdecls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let loc = { loc with loc_ghost = true } in
  match tdecls with
  | [ { ptype_name = { txt = tname; _ }; _ } ] ->
      let typ = Ast_builder.Default.ptyp_constr ~loc (Located.mk ~loc (Lident tname)) [] in
      [%str module Base = struct type t = [%t typ] end]
  | _ ->
      Location.raise_errorf ~loc
        "base_module can be derived for a single type only"

let () =
  Deriving.add "base_module"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore
```

Ключевые моменты:

- `[%str module Base = struct type t = [%t typ] end]` — это quotation для `structure` (возвращает `structure_item list`), внутри неё `[%t typ]` — антиквотация, подставляющая `core_type`
- `typ` строится через `ptyp_constr` — это единственное, что нельзя закавотить, потому что имя типа динамическое
- Убрана проверка на `Nonrecursive` — тебе она не нужна, ты просто делаешь алиас
- Параметры типа игнорируются — алиас `type t = nat` без параметров

Если тип параметризован (например `type 'a foo = ...`), нужно ли генерировать `type 'a t = 'a foo`? Сейчас генерируется просто `type t = foo` без параметров.

### да, нужно с параметом, дай пример как с параметром



Для `type ('a, 'b) foo = ... [@@deriving base_module]` нужно сгенерировать:

```ocaml
module Base = struct type ('a, 'b) t = ('a, 'b) foo end
```

Проблема в том, что `[%str ...]` не позволяет антиквотировать параметры type declaration (`('a, 'b)` слева от `t`) — для этого нет антиквотации в metaquot. Поэтому придётся собрать `type_declaration` руками, а модуль оставить через quotation:

```ocaml
open Ppxlib

let str_type_decl ~ctxt (_rec_flag, tdecls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let loc = { loc with loc_ghost = true } in
  let open Ast_builder.Default in
  match tdecls with
  | [ { ptype_name = { txt = tname; _ }; ptype_params = params; _ } ] ->
      let args = List.map (fun (p, _) -> p) params in
      let manifest = ptyp_constr ~loc (Located.mk ~loc (Lident tname)) args in
      let td =
        type_declaration ~loc ~name:(Located.mk ~loc "t") ~params ~cstrs:[]
          ~kind:Ptype_abstract ~private_:Public ~manifest:(Some manifest)
      in
      [%str module Base = struct [%%i pstr_type ~loc Nonrecursive [ td ]] end]
  | _ ->
      Location.raise_errorf ~loc
        "base_module can be derived for a single type only"

let () =
  Deriving.add "base_module"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg str_type_decl)
  |> Deriving.ignore
```

Здесь `[%%i expr]` — антиквотация для `structure_item`, подставляет собранный `pstr_type` внутрь тела модуля. Параметры (`params`) берутся прямо из исходного типа и переиспользуются как есть.
 *)
