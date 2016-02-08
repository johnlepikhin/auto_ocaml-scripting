
open ScriptParse

let empty =
  let world = ScriptInterp.{
      external_fns = [];
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

let world_of_externals ?initial ?(additional="") fns =
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
          external_fns = world.external_fns @ fns;
        } in
      world, env
  in
  let script = List.map script_of_external fns |> String.concat "" in
  let script = script ^ "\n" ^ additional in
  print_endline script;
  let script = init ~env ~fileName:"external_functions" ~moduleName:"External" script in
  let parsed = parse script in
  let compiled = compile parsed in
  let state = ScriptInterp.init ~world ~stackSize:1000 compiled.instr in
  let open ScriptInterp in
  interp state;
  state.world, parsed.source.env

external set_id: 'a -> 'a = "caml_set_oo_id" [@@noalloc]
