
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

let parse source =
  try
    let lexbuf = Lexing.from_string source.script in
    let tree = Parse.implementation lexbuf in
    let loc = Location.in_file source.fileName in
    let (str, sg, newenv) = Typemod.type_structure source.env tree loc in
    let lambda = Translmod.transl_implementation source.moduleName (str, Typedtree.Tcoerce_none) in
    let lambda = Simplif.simplify_lambda lambda in
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
      | _ ->
        raise exn
    in
    Format.pp_print_flush fmt ();
    let error = Buffer.contents b in
    raise (Error (error, loc))

let compile t =
  let instr = Bytegen.compile_implementation t.source.moduleName t.lambda in
  {
    parsed = t;
    instr;
  }

let env_basic source =
  let script = "

external raise : exn -> 'a = \"%raise\"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Composition operators *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = \"%revapply\"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = \"%apply\"

(* Comparisons *)

external ( = ) : 'a -> 'a -> bool = \"%equal\"
external ( <> ) : 'a -> 'a -> bool = \"%notequal\"
external ( < ) : 'a -> 'a -> bool = \"%lessthan\"
external ( > ) : 'a -> 'a -> bool = \"%greaterthan\"
external ( <= ) : 'a -> 'a -> bool = \"%lessequal\"
external ( >= ) : 'a -> 'a -> bool = \"%greaterequal\"
external compare : 'a -> 'a -> int = \"%compare\"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external ( == ) : 'a -> 'a -> bool = \"%eq\"
external ( != ) : 'a -> 'a -> bool = \"%noteq\"

(* Boolean operations *)

external not : bool -> bool = \"%boolnot\"
external ( & ) : bool -> bool -> bool = \"%sequand\"
external ( && ) : bool -> bool -> bool = \"%sequand\"
external ( or ) : bool -> bool -> bool = \"%sequor\"
external ( || ) : bool -> bool -> bool = \"%sequor\"

(* Integer operations *)

external ( ~- ) : int -> int = \"%negint\"
external ( ~+ ) : int -> int = \"%identity\"
external succ : int -> int = \"%succint\"
external pred : int -> int = \"%predint\"
external ( + ) : int -> int -> int = \"%addint\"
external ( - ) : int -> int -> int = \"%subint\"
external ( *  ) : int -> int -> int = \"%mulint\"
external ( / ) : int -> int -> int = \"%divint\"
external ( mod ) : int -> int -> int = \"%modint\"

let abs x = if x >= 0 then x else -x

external ( land ) : int -> int -> int = \"%andint\"
external ( lor ) : int -> int -> int = \"%orint\"
external ( lxor ) : int -> int -> int = \"%xorint\"

let lnot x = x lxor (-1)

external ( lsl ) : int -> int -> int = \"%lslint\"
external ( lsr ) : int -> int -> int = \"%lsrint\"
external ( asr ) : int -> int -> int = \"%asrint\"

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62)
let max_int = min_int - 1

(* Floating-point operations *)

external ( ~-. ) : float -> float = \"%negfloat\"
external ( ~+. ) : float -> float = \"%identity\"
external ( +. ) : float -> float -> float = \"%addfloat\"
external ( -. ) : float -> float -> float = \"%subfloat\"
external ( *. ) : float -> float -> float = \"%mulfloat\"
external ( /. ) : float -> float -> float = \"%divfloat\"
external abs_float : float -> float = \"%absfloat\"
external float : int -> float = \"%floatofint\"
external float_of_int : int -> float = \"%floatofint\"
external truncate : float -> int = \"%intoffloat\"
external int_of_float : float -> int = \"%intoffloat\"

(* String operations -- more in module String *)

external string_length : string -> int = \"%string_length\"

(* Character operations -- more in module Char *)

external int_of_char : char -> int = \"%identity\"

(* Unit operations *)

external ignore : 'a -> unit = \"%ignore\"

(* Pair operations *)

external fst : 'a * 'b -> 'a = \"%field0\"
external snd : 'a * 'b -> 'b = \"%field1\"

"
  in
  let parsed = parse { source with script } in
  parsed.source
