
open ScriptParse

let initial = init ~fileName:"main" ~moduleName:"Main" ""

let with_basic = env_basic initial

let () =
  let script = "let a = 1 + 2 - 3*4/2;;

    "
  in
  let source = { with_basic with script } in
  let parsed = parse source in
  let compiled = compile parsed in
  let state = ScriptInterp.init ~stackSize:1000 compiled.instr in
  ScriptInterp.interp state

