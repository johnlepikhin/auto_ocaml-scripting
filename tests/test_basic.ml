
open ScriptParse

let external_fns = ScriptInterp.[
    ext_fn "pcre_regexp" ["string"] "regexp" (Obj.repr ( Pcre.regexp ));
    ext_fn "pcre_match" ["regexp"; "string"] "bool" (Obj.repr ( Pcre.pmatch ));
    ext_fn "print" ["string"] "unit" (Obj.repr ( print_endline ));
]

let (world, env) =
  ScriptExternal.world_of_externals
    ~prefix:"type regexp"
    external_fns

let script1 = init ~fileName:"script1" "

let rex = pcre_regexp \"substring\";;
"

let script2 = init ~fileName:"script2" "

let () =
    if pcre_match rex \"111 substring 222\" then
      print \"match!\"
"


let () =
  wrapped ~error_cb:(Printf.printf "%s\n") (fun () ->
      let parsed = parse ~initial_env:env ~moduleName:"Main" [script1; script2] in
      let compiled = compile parsed in
      let state = ScriptInterp.init ~world ~stackSize:1000 compiled.instr in
      ScriptInterp.interp state
    )
