
open ScriptParse

let script = "

exception Exit

external print_endline: string -> unit = \"print_endline\"

let () =
  try
    raise Exit
  with
  | _ -> print_endline \"cached!\"
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

let initial = init ~fileName:"main" ~moduleName:"Main" ""

let with_basic = env_basic initial

let () =
  let source = { with_basic with script } in
  let parsed = parse source in
  print_lambda parsed.lambda;
  let compiled = compile parsed in
  let open ScriptInterp in
  let state = init ~stackSize:1000 compiled.instr in
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
