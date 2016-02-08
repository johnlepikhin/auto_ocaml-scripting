
open ScriptParse

let external_fns = ScriptInterp.[
    ext_fn "pcre_regexp" ["string"] "regexp" (Obj.repr (fun rex -> Pcre.regexp rex));
    ext_fn "pcre_regexp_or" ["string list"] "regexp" (Obj.repr ( Pcre.regexp_or ));
    ext_fn "pcre_match" ["regexp"; "string"] "bool" (Obj.repr (fun rex s -> Pcre.pmatch ~rex s));
    ext_fn "print" ["string"] "unit" (Obj.repr print_endline);
]

let initial = ScriptHelpers.addComparsions ScriptExternal.empty

let (world, env) =
  ScriptExternal.world_of_externals ~initial
    ~prefix:"type regexp"
    external_fns

let script = init ~env ~fileName:"main" ~moduleName:"Main" "

let pmatch rex =
    let rex1 = pcre_regexp rex in
    fun s ->
      pcre_match rex1 s
;;

if pmatch \".*?substr\" \"substring\" then
 print \"match\"
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
