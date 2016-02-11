
exception Error of string * Location.t

type source = {
  script : string;
  env : Env.t;
  moduleName : string;
  fileName : string;
}

type parsed = {
  source : source;
  lambda : Lambda.lambda;
}

type compiled = {
  parsed : parsed;
  instr : Instruct.instruction list;
}

let init ?(env=Env.initial_unsafe_string) ~fileName ~moduleName script = {
  script;
  env;
  moduleName;
  fileName;
}

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

let parse ?(debug=false) ?mapper source =
  try
    let lexbuf = Lexing.from_string source.script in
    Location.init lexbuf source.fileName;
    let loc = Location.curr lexbuf in
    let tree = Parse.implementation lexbuf in
    let tree =
      match mapper with
      | None -> tree
      | Some mapper ->
        mapper.Ast_mapper.structure mapper tree
    in
    let (str, _, newenv) = Typemod.type_structure source.env tree loc in
    let lambda = Translmod.transl_implementation source.moduleName (str, Typedtree.Tcoerce_none) in
    let lambda = Simplif.simplify_lambda lambda in
    if debug then
      print_lambda lambda;
    {
      source = { source with env = newenv };
      lambda;
    }
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
      | Location.Error error ->
        Location.report_error fmt error;
        error.Location.loc
      | _ ->
        raise exn
    in
    Format.pp_print_flush fmt ();
    let error = Buffer.contents b in
    raise (Error (error, loc))

let compile ?(debug=false) t =
  let instr = Bytegen.compile_implementation t.source.moduleName t.lambda in
  if debug then
    print_instr instr;
  {
    parsed = t;
    instr;
  }

let wrapped ~error_cb fn =
  try
    fn ()
  with
  | Error (msg, loc) ->
    let open Location in
    let (file, bline, bchar) = get_pos_info loc.loc_start in
    let (_, eline, echar) = get_pos_info loc.loc_end in
    let msg =
      Printf.sprintf "Error in %s at line %i, chars %i-%i: %s" file bline bchar echar msg
    in
    error_cb msg
