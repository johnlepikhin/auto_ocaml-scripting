
open ScriptParse

let empty =
  let world = ScriptInterp.{
      external_fns = ScriptInterp.ExtMap.empty;
      global = Global.init ();
    }
  in
  world, Env.initial_unsafe_string

let script_of_external fn =
  let open ScriptInterp in
  let args = String.concat " -> " fn.fn_args in
  let ext_name =
    match fn.fn_ext_name with
    | None -> fn.fn_name
    | Some v -> v
  in
  Printf.sprintf "external %s: %s -> %s = \"%s\";;\n" fn.fn_name args fn.fn_return ext_name

let world_of_externals ?initial ?(prefix="") ?(suffix="") fns =
  let fns = List.fold_left (fun r fn ->
      ScriptInterp.ExtMap.add fn.ScriptInterp.fn_name fn r
    ) ScriptInterp.ExtMap.empty fns
  in
  let world, env = match initial with
    | None ->
      let world = ScriptInterp.{
          external_fns = fns;
          global = Global.init ();
        }
      in
      world, Env.initial_unsafe_string
    | Some (world, env) ->
      let world = ScriptInterp.{
          world with
          external_fns = ScriptInterp.ExtMap.concat world.external_fns fns;
        } in
      world, env
  in
  let script =
    ScriptInterp.ExtMap.bindings fns
    |> List.map (fun (k, v) -> script_of_external v)
    |> String.concat ""
  in
  let script = prefix ^ "\n" ^ script ^ "\n" ^ suffix in
  let script = init ~env ~fileName:"external_functions" ~moduleName:"External" script in
  let parsed = parse script in
  let compiled = compile parsed in
  let state = ScriptInterp.init ~world ~stackSize:1000 compiled.instr in
  let open ScriptInterp in
  interp state;
  state.world, parsed.source.env

external set_id: 'a -> 'a = "caml_set_oo_id" [@@noalloc]
