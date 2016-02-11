
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let print_lambda t =
  print_endline "-- Lambda -----------------------";
  let b = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer b in
  Printlambda.lambda fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents b |> print_endline

module Mapper =
struct
  let exp_construct loc n v = Exp.construct ~loc (Location.mkloc (Lident n) loc) v

  let loc_error ~loc msg =
    let err = Location.error ~loc msg in
    raise (Location.Error err)

  let rec list_of_strings loc name = function
    | [] ->
      [exp_construct loc "[]" None]

    | (_, ({ pexp_desc = Pexp_constant (Const_string (_, _));
             pexp_loc = loc
           } as v)) :: tl ->
      [exp_construct loc "::" (Some (
           Exp.tuple ~loc (v :: list_of_strings loc name tl))
         )]
    | _ ->
      let msg = name ^ " accepts list of strings: " ^ name ^ " \"string1\" \"string2\" ..." in
      loc_error ~loc msg

  let generator =
    let id = ref 0 in
    fun () ->
      incr id;
      Printf.sprintf "__my%i" !id

  let rec my_mapper =
    let regexps = ref [] in
    let expr_mapper mapper = function
      | {
        pexp_desc = Pexp_apply ({
            pexp_desc = Pexp_ident {txt = Lident "regexp"}
          }, args);
        pexp_loc = loc;
      }
        ->
        let name = generator () |> Printf.sprintf "__regexp%s" in
        let info = loc, args, name in
        regexps := info :: !regexps;
        let apply_name = Exp.ident ~loc (Location.mkloc (Lident "pmatch") loc) in
        let rex_name = Exp.ident ~loc (Location.mkloc (Lident name) loc) in
        Exp.apply ~loc apply_name ["rex", rex_name]

      | x -> default_mapper.expr mapper x;
    in
    let structure_mapper mapper items =
      let items = List.map (fun v -> default_mapper.structure_item mapper v) items in

      let make_rex (loc, args, name) =
        let rexname = Exp.ident (Location.mkloc (Lident "pcre_regexp_or") loc) in
        let regexps =
          list_of_strings loc "regexp" args
          |> List.map (fun v -> "", v)
        in
        let expr = Exp.apply rexname regexps in
        Vb.mk (Pat.var (Location.mkloc name loc)) expr
      in
      let my_bindings =
        List.map make_rex !regexps
        |> Str.value Nonrecursive
      in

      my_bindings :: items
    in

    { Ast_mapper.default_mapper with
      expr = expr_mapper;
      structure = structure_mapper;
    }

end

open ScriptParse

let external_fns = ScriptInterp.[
    ext_fn "print" ["string"] "unit" (Obj.repr ( print_endline ));
    ext_fn "pcre_regexp_or" ["string list"] "regexp" (Obj.repr (fun lst ->
        print_endline "pcre_regexp_or vals: ";
        List.iter print_endline lst;
        String.concat "|" lst
      ));
    ext_fn "pmatch" ["rex : regexp"] "unit" (Obj.repr (fun regexp ->
        print_endline ("pmatch regexp: " ^ regexp)
      ));
]

let (world, env) = ScriptExternal.world_of_externals ~prefix:"type regexp" external_fns

let script = init ~fileName:"main" "

let r = regexp \"asd\" \"zzz\"
;;

"


let () =
  let parsed = parse ~initial_env:env ~mapper:Mapper.my_mapper ~moduleName:"Test" [script] in
  let compiled = compile parsed in
  let state = ScriptInterp.init ~world ~stackSize:1000 compiled.instr in
  ScriptInterp.interp state

