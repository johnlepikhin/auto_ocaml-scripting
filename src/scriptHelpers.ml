
let addExceptions initial = ScriptExternal.world_of_externals ~initial ScriptInterp.[
    ext_fn ~fn_ext_name:"%raise" "raise" ["exn"] "'a" (Obj.repr print_endline);
    ext_fn "caml_set_oo_id" ["exn"] "'a" (Obj.repr ScriptExternal.set_id);
]

let addCompositions initial = ScriptExternal.world_of_externals ~initial ScriptInterp.[
    ext_fn ~fn_ext_name:"%revapply" "( |> )" ["'a"; "('a -> 'b)"] "'b" (Obj.repr ( |> ));
    ext_fn ~fn_ext_name:"%apply" "( @@ )" ["('a -> 'b)"; "'a"] "'b" (Obj.repr ( @@ ));
  ]    

let addComparsions initial =
  let additional = "
let min x y = if x <= y then x else y
let max x y = if x >= y then x else y
"    
  in
  ScriptExternal.world_of_externals ~initial ~additional ScriptInterp.[
      ext_fn ~fn_ext_name:"%equal" "( = )" ["'a"; "'a"] "bool" (Obj.repr ( = ));
      ext_fn ~fn_ext_name:"%notequal" "( <> )" ["'a"; "'a"] "bool" (Obj.repr ( <> ));
      ext_fn ~fn_ext_name:"%lessthan" "( < )" ["'a"; "'a"] "bool" (Obj.repr ( < ));
      ext_fn ~fn_ext_name:"%greaterthan" "( > )" ["'a"; "'a"] "bool" (Obj.repr ( > ));
      ext_fn "( <= )" ["'a"; "'a"] "bool" (Obj.repr ( <= ));
      ext_fn "( >= )" ["'a"; "'a"] "bool" (Obj.repr ( >= ));
      ext_fn ~fn_ext_name:"%compare" "compare" ["'a"; "'a"] "int" (Obj.repr ( compare ));
      ext_fn "caml_string_equal" ["string"; "string"] "bool" (Obj.repr ( = ));
      ext_fn "caml_string_greaterthan" ["string"; "string"] "bool" (Obj.repr ( > ));
      ext_fn "caml_string_lessthan" ["string"; "string"] "bool" (Obj.repr ( < ));
      ext_fn "caml_string_greaterequal" ["string"; "string"] "bool" (Obj.repr ( >= ));
      ext_fn "caml_string_lessequal" ["string"; "string"] "bool" (Obj.repr ( <= ));

      ext_fn ~fn_ext_name:"%eq" "( == )" ["bool"; "bool"] "bool" (Obj.repr ( == ));
      ext_fn ~fn_ext_name:"%noteq" "( != )" ["bool"; "bool"] "bool" (Obj.repr ( != ));

  ]    

let addBoolean initial =
  ScriptExternal.world_of_externals ~initial ScriptInterp.[
      ext_fn ~fn_ext_name:"%boolnot" "not" ["bool"; "bool"] "bool" (Obj.repr not);
      ext_fn ~fn_ext_name:"%sequand" "( && )" ["bool"; "bool"] "bool" (Obj.repr ( && ));
      ext_fn ~fn_ext_name:"%sequor" "( || )" ["bool"; "bool"] "bool" (Obj.repr ( || ));
    ]

let addIntegers initial =
  let additional = "
let abs x = if x >= 0 then x else -x

"
  in
  ScriptExternal.world_of_externals ~initial ~additional ScriptInterp.[
      ext_fn ~fn_ext_name:"%negint" "( ~- )" ["int"] "int" (Obj.repr ( ~- ));
      ext_fn ~fn_ext_name:"%identity" "( ~+ )" ["int"] "int" (Obj.repr ( ~+ ));
      ext_fn ~fn_ext_name:"%succint" "succ" ["int"] "int" (Obj.repr ( succ ));
      ext_fn ~fn_ext_name:"%predint" "pred" ["int"] "int" (Obj.repr ( pred ));
      ext_fn ~fn_ext_name:"%addint" "( + )" ["int"; "int"] "int" (Obj.repr ( + ));
      ext_fn ~fn_ext_name:"%subint" "( - )" ["int"; "int"] "int" (Obj.repr ( - ));
      ext_fn ~fn_ext_name:"%mulint" "( * )" ["int"; "int"] "int" (Obj.repr ( * ));
      ext_fn ~fn_ext_name:"%divint" "( / )" ["int"; "int"] "int" (Obj.repr ( / ));
      ext_fn ~fn_ext_name:"%modint" "( mod )" ["int"; "int"] "int" (Obj.repr ( mod ));
    ]
