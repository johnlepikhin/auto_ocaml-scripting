
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let getenv s = try Sys.getenv s with Not_found -> ""

let mapper =
  { Ast_mapper.default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "getenv"; loc }, pstr)} -> (
          match pstr with
          | PStr [{ pstr_desc =
                      Pstr_eval (
                        { pexp_loc  = loc;
                          pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
            Exp.constant ~loc (Const_string (getenv sym, None))
          | _ ->
            raise (Location.Error (
                Location.error ~loc "[%getenv] accepts a string, e.g. [%getenv \"USER\"]"))
        )
      | x -> default_mapper.expr mapper x;
  }

open ScriptParse

let external_fns = ScriptInterp.[
    ext_fn "print" ["string"] "unit" (Obj.repr ( print_endline ));
]

let (world, env) = ScriptExternal.world_of_externals external_fns

let script = init ~env ~fileName:"main" ~moduleName:"Main" "

let () =
    print [%getenv \"USER\"]
"


let () =
  let parsed = parse ~mapper script in
  let compiled = compile parsed in
  let state = ScriptInterp.init ~world ~stackSize:1000 compiled.instr in
  ScriptInterp.interp state

