
open ScriptParse

let world, env = ScriptExternal.world_of_externals ScriptInterp.[
    { fn_name = "print_endline";
      fn_args = ["string"];
      fn_return = "unit";
      fn = Obj.repr print_endline;
      fn_ext_name = None;
    };
]

let world, env =
  ScriptHelpers.addComparsions (world, env)
  |> ScriptHelpers.addExceptions
  |> ScriptHelpers.addIntegers

let script = init ~env ~fileName:"main" ~moduleName:"Main" "

exception Exit

let () =
  try
    let z = 1 + 2 in
    print_endline \"before raising exception\";
    raise Exit
  with
  | Exit -> print_endline \"catched!\"

"

let print_lambda t =
  print_endline "-- Lambda -----------------------";
  let b = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer b in
  Printlambda.lambda fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents b |> print_endline

let print_instr t =
  print_endline "-- Instructions -----------------------";
  let b = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer b in
  Printinstr.instrlist fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents b |> print_endline

let () =
  let parsed = parse script in
  print_lambda parsed.lambda;
  let compiled = compile parsed in
  let open ScriptInterp in
  let state = init ~world ~stackSize:1000 compiled.instr in
  let codelen = Array.length state.code in
  let instr = Bytegen.compile_implementation parsed.source.moduleName parsed.lambda in
  print_instr instr;
  reset state;
  while state.pc < codelen do
    Printf.printf "--- pc=%i/%i, sp=%i, extraArgs=%i ---\n"
      state.pc
      codelen
      state.sp
      state.extraArgs;
    flush_all ();

    step state;
  done
