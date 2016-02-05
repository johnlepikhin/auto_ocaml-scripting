
exception Error of string * Location.t

let parse ?(env=Env.initial_unsafe_string) ~fileName ~moduleName s =
  try
    let lexbuf = Lexing.from_string s in
    let tree = Parse.implementation lexbuf in
    let loc = Location.in_file fileName in
    let (str, sg, finalenv) = Typemod.type_structure env tree loc in
    let lambda = Translmod.transl_implementation moduleName (str, Typedtree.Tcoerce_none) in
    let lambda = Simplif.simplify_lambda lambda in
    lambda, env
  with
  | exn ->
    let b = Buffer.create 100 in
    let fmt = Format.formatter_of_buffer b in
    let loc =
      match exn with
      | Syntaxerr.Error err ->
        Syntaxerr.report_error fmt err;
        Syntaxerr.location_of_error err
      | Typetexp.Error (loc, env, err) ->
        Typetexp.report_error env fmt err;
        loc
      | Typecore.Error (loc, env, err) ->
        Typecore.report_error env fmt err;
        loc
      | _ ->
        raise exn
    in
    Format.pp_print_flush fmt ();
    let error = Buffer.contents b in
    raise (Error (error, loc))
