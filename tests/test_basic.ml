
open ScriptParse

let external_fns = ScriptInterp.[
    { fn_name = "print_endline";
      fn_args = ["string"];
      fn_return = "unit";
      fn = Obj.repr print_endline;
      fn_ext_name = None;
    };
]

let (world, env) = ScriptExternal.world_of_externals external_fns

let script = init ~env ~fileName:"main" ~moduleName:"Main" "

print_endline \"test\";

"


let () =
  let parsed = parse script in
  let compiled = compile parsed in
  let state = ScriptInterp.init ~world ~stackSize:1000 compiled.instr in
  ScriptInterp.interp state

