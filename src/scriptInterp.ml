
exception Stack_overflow
exception Raise_exn of string
exception Error of string

let fail s =
  raise (Error s)

module Global =
struct
  module StringMap = Map.Make(String)

  type t = (Ident.t, Obj.t) Hashtbl.t

  let get global ident =
    Obj.repr (Hashtbl.find global ident)

  let set global ident block =
    Hashtbl.replace global ident block

  let init () =
    let global : t = Hashtbl.create 101 in
    let toplevel_value_bindings : Obj.t StringMap.t ref = ref StringMap.empty in
    let getvalue name =
      try
        StringMap.find name !toplevel_value_bindings
      with Not_found ->
        fail (name ^ " unbound at toplevel")
    in
    let setvalue name v =
      toplevel_value_bindings := StringMap.add name v !toplevel_value_bindings
    in
    let toploop = Obj.repr (getvalue, setvalue) in
    Hashtbl.add global (Ident.create "Toploop") toploop;
    global
end

type pc = int

type sp = int

type external_fn = {
  fn_name : string;
  fn_args: string list;
  fn_return: string;
  fn : Obj.t;
  fn_ext_name : string option;
}

let ext_fn ?fn_ext_name fn_name fn_args fn_return fn = {
  fn_name;
  fn_args;
  fn_return;
  fn;
  fn_ext_name;
}

type world = {
  external_fns : external_fn list;
  mutable global : Global.t;
}

type state = {
  code : Instruct.instruction array;
  mutable pc : pc;
  stack : Obj.t array;
  mutable sp : sp;
  mutable accu : Obj.t;
  mutable trapSp : sp;
  mutable extraArgs : int;
  mutable env : Obj.t;
  labels : pc array;
  world : world;
}

let rec value_of_constant v =
  let open Lambda in
  match v with
  | Const_pointer v ->
    Obj.repr v
  | Const_base v ->
    Asttypes.(
      match v with
      | Const_int v ->
        Obj.repr v
      | Const_char v ->
        Obj.repr v
      | Const_string (v, _) ->
        Obj.repr v
      | Const_float v ->
        Obj.repr v
      | Const_int32 v ->
        Obj.repr v
      | Const_int64 v ->
        Obj.repr v
      | Const_nativeint v ->
        Obj.repr v
    )
  | Const_block (tag, lst) ->
    let len = List.length lst in
    let block = Obj.new_block tag len in
    let rec aux pos = function
      | [] -> ()
      | v :: tl ->
        let v = value_of_constant v in
        Obj.set_field block pos v;
        aux (pos+1) tl
    in
    aux 0 lst;
    block
  | Const_immstring s ->
    Obj.repr s
  | Const_float_array lst ->
    let len = List.length lst in
    let block = Obj.new_block Obj.double_array_tag len in
    let rec aux pos = function
      | [] -> ()
      | v :: tl ->
        Obj.set_field block pos (Obj.repr v);
        aux (pos+1) tl
    in
    aux 0 lst;
    block

let init ~stackSize ~world instr =
  let lst = ref [] in
  let rec aux pos = function
    | [] -> []
    | Instruct.Klabel l :: tl ->
      lst := (l, pos) :: !lst;
      aux pos tl
    | Instruct.Kcheck_signals :: tl ->
      aux pos tl
    | Instruct.Kconst v :: tl ->
      let v = value_of_constant v in
      Instruct.Kconst (Obj.magic v) :: aux (pos+1) tl
    | Instruct.Kintcomp v :: tl ->
      let op =
        let open Lambda in
        match v with
        | Ceq -> ( = )
        | Cneq -> ( <> )
        | Clt -> ( < )
        | Cgt -> ( > )
        | Cle -> ( <= )
        | Cge -> ( >= )
      in
      Instruct.Kintcomp (Obj.magic op) :: aux (pos+1) tl

    | Instruct.Kccall ("raise", args) :: tl -> (
        Instruct.Kraise Lambda.Raise_notrace :: aux (pos+1) tl
      )
    | Instruct.Kccall (fn, args) :: tl -> (
        try
          let fn = List.find (fun efn -> efn.fn_name = fn) world.external_fns in
          if List.length fn.fn_args <> args then (
            Printf.sprintf "Invalid number of arguments for external function: %s" fn.fn_name
            |> fail
          );
          Instruct.Kccall ((Obj.magic fn.fn), args) :: aux (pos+1) tl
        with
        | Not_found ->
          Printf.sprintf "Undefined external function: %s" fn
          |> fail
      )

    | hd :: tl ->
      hd :: aux (pos+1) tl
  in
  let newinstr = aux 0 instr in
  let len = List.length !lst in
  let labels = Array.make len 0 in
  List.iter (fun (lbl, pc) -> Array.unsafe_set labels (lbl-1) pc) !lst;
  let stack = Array.init stackSize (fun id -> Obj.repr ()) in
  {
    code = Array.of_list newinstr;
    pc = 0;
    stack;
    sp = stackSize - 1;
    accu = Obj.repr ();
    trapSp = -1;
    extraArgs = 0;
    env = Obj.repr ();
    labels;
    world;
  }

let reset state =
  state.pc <- 0;
  state.sp <- (Array.length state.stack) - 1;
  state.accu <- Obj.repr ();
  state.extraArgs <- 0;
  state.env <- Obj.repr ()

let step state =
  let open Instruct in
  let module A = Array in
  let stack_push v =
    if state.sp = 0 then (
      raise Stack_overflow;
    ) else (
      state.sp <- state.sp - 1;
      A.unsafe_set state.stack state.sp v
    )
  in
  match Array.unsafe_get state.code state.pc with
  | Instruct.Kacc n ->
    state.accu <- A.unsafe_get state.stack (state.sp + n);
    state.pc <- state.pc + 1;

  | Instruct.Kpush ->
    stack_push state.accu;
    state.pc <- state.pc + 1;

  | Instruct.Kpop n ->
    state.sp <- state.sp + 1;
    state.pc <- state.pc + 1;

  | Instruct.Kassign pos ->
    A.unsafe_set state.stack (state.sp + pos) state.accu;
    state.accu <- Obj.repr ();
    state.pc <- state.pc + 1;

  | Instruct.Kenvacc pos ->
    state.accu <- Obj.field state.env pos;
    state.pc <- state.pc + 1;

  | Instruct.Kpush_retaddr lbl ->
    state.sp <- state.sp - 3;
    (*
    print_endline "vvvv TODO TODO TODO TODO vvvvv";
*)
    A.unsafe_set state.stack state.sp (Obj.repr (state.labels.(lbl-1)));
    state.pc <- state.pc + 1;

  | Instruct.Kapply n when n < 4 ->
    let args = A.sub state.stack state.sp n in
    state.sp <- state.sp - 3;

    A.blit args 0 state.stack state.sp n;

    A.unsafe_set state.stack (state.sp + n) (Obj.repr (state.pc +1));
    A.unsafe_set state.stack (state.sp + n + 1) state.env;
    A.unsafe_set state.stack (state.sp + n + 2) (Obj.repr state.extraArgs);
    state.pc <- Obj.obj (Obj.field state.accu 0);
    state.env <- state.accu;
    state.extraArgs <- n - 1;

  | Instruct.Kapply n ->
    state.extraArgs <- n - 1;
    state.pc <- Obj.obj (Obj.field state.accu 0);
    state.env <- state.accu;

  | Instruct.Kreturn n ->
    state.sp <- state.sp + n;
    if state.extraArgs > 0 then (
      state.extraArgs <- state.extraArgs - 1;
      state.pc <- Obj.obj (Obj.field state.accu 0);
      state.env <- state.accu
    ) else (
      state.pc <- Obj.obj (A.unsafe_get state.stack state.sp);
      state.env <- A.unsafe_get state.stack (state.sp + 1);
      state.extraArgs <- Obj.obj (A.unsafe_get state.stack (state.sp + 2));
      state.sp <- state.sp + 3;
    );

  | Instruct.Krestart ->
    let num_args = (Obj.size state.env) - 2 in
    state.sp <- state.sp - num_args;
    for i=0 to num_args-1 do
      A.unsafe_set state.stack (state.sp + i) (Obj.field state.env (i+2))
    done;
    state.env <- Obj.field state.env 1;
    state.extraArgs <- state.extraArgs + num_args;
    state.pc <- state.pc + 1;

  | Instruct.Kgrab required ->
    if state.extraArgs >= required then (
      state.extraArgs <- state.extraArgs - required;
      state.pc <- state.pc + 1;
    ) else (
      let num_args = 1 + state.extraArgs in
      state.accu <- Obj.new_block Obj.closure_tag (num_args + 2);
      Obj.set_field state.accu 1 state.env;
      for i=0 to num_args-1 do
        Obj.set_field state.accu (i+2) (A.unsafe_get state.stack (state.sp + i))
      done;
      Obj.set_field state.accu 0 (Obj.repr (state.pc - 1));
      state.sp <- state.sp + num_args;
      state.pc <- Obj.obj (A.unsafe_get state.stack state.sp);
      state.env <- A.unsafe_get state.stack (state.sp+1);
      state.extraArgs <- Obj.obj (A.unsafe_get state.stack (state.sp+2));
      state.sp <- state.sp + 3;
    )

  | Instruct.Kclosure (lbl, nvars) ->
    if nvars > 0 then
      stack_push state.accu;
    state.accu <- Obj.new_block Obj.closure_tag (1 + nvars);
    for i=0 to nvars-1 do
      Obj.set_field state.accu (i+1) (A.unsafe_get state.stack i);
    done;
    Obj.set_field state.accu 0 (Obj.repr state.labels.(lbl-1));
    state.sp <- state.sp + nvars;
    state.pc <- state.pc + 1;

  | Instruct.Ksetglobal id ->
    Global.set state.world.global id state.accu;
    state.accu <- Obj.repr ();
    state.pc <- state.pc + 1;
    
  | Instruct.Kbranch lbl ->
    state.pc <- state.labels.(lbl-1)

  | Instruct.Kbranchif lbl ->
    if Obj.obj state.accu <> false then
      state.pc <- state.labels.(lbl-1)
    else
      state.pc <- state.pc + 1;

  | Instruct.Kbranchifnot lbl ->
    if Obj.obj state.accu = false then
      state.pc <- state.labels.(lbl-1)
    else
      state.pc <- state.pc + 1;

  | Instruct.Kconst v ->
    state.accu <- Obj.magic v;
    state.pc <- state.pc + 1;

  | Instruct.Kccall (fn, params) ->
    let result =
      let arg1 = state.accu in
      match params with
      | 1 ->
        let fn : 'a -> 'b = Obj.magic fn in
        Obj.repr (fn (Obj.obj arg1))
      | 2 -> (
          let arg2 = A.unsafe_get state.stack state.sp in
          state.sp <- state.sp + 1;
          let fn : 'a -> 'b -> 'c = Obj.magic fn in
          Obj.repr (fn (Obj.obj arg1) (Obj.obj arg2))
          )
      | 3 -> (
          let arg2 = A.unsafe_get state.stack state.sp in
          let arg3 = A.unsafe_get state.stack state.sp in
          state.sp <- state.sp + 2;
          let fn : 'a -> 'b -> 'c -> 'd = Obj.magic fn in
          Obj.repr (fn (Obj.obj arg1) (Obj.obj arg2) (Obj.obj arg3))
          )
      | _ ->
        fail "External functions with arity > 3 are not supported"
    in
    state.accu <- result;
    state.pc <- state.pc + 1;

  | Instruct.Kmakeblock (wosize, tag) ->
    let block = Obj.new_block tag wosize in
    Obj.set_field block 0 state.accu;
    for i=1 to wosize-1 do
      Obj.set_field block i (A.unsafe_get state.stack state.sp);
      state.sp <- state.sp + 1;
    done;
    state.accu <- block;
    state.pc <- state.pc + 1;

  | Instruct.Kgetfield pos ->
    state.accu <- Obj.field state.accu pos;
    state.pc <- state.pc + 1;

  | Instruct.Kintcomp op ->
    let op : int -> int -> bool = Obj.magic op in
    state.accu <- Obj.repr (op (Obj.obj state.accu) (Obj.obj (A.unsafe_get state.stack state.sp)));
    state.sp <- state.sp + 1;
    state.pc <- state.pc + 1;

  | Instruct.Koffsetint ofs ->
    state.accu <- Obj.repr ((Obj.obj state.accu) + ofs);
    state.pc <- state.pc + 1;

  | Instruct.Klabel _ ->
    fail "unexpected label opcode"

  | Instruct.Kcheck_signals ->
    state.pc <- state.pc + 1;

  | Instruct.Kraise kind ->
    if state.trapSp >= 0 then (
      state.sp <- state.trapSp;
      state.pc <- Obj.obj (A.get state.stack state.sp);
      state.trapSp <- Obj.obj (A.get state.stack (state.sp+1));
      state.env <- Obj.obj (A.get state.stack (state.sp+2));
      state.extraArgs <- Obj.obj (A.get state.stack (state.sp+3));
      state.sp <- state.sp + 4
    ) else (
      raise (Raise_exn ("Raised exception: " ^ (Obj.obj (Obj.field state.accu 0))))
    )

  | Instruct.Kpushtrap lbl ->
    state.sp <- state.sp - 4;
    A.unsafe_set state.stack state.sp (Obj.repr (state.labels.(lbl-1)));
    A.unsafe_set state.stack (state.sp + 1) (Obj.repr state.trapSp);
    A.unsafe_set state.stack (state.sp + 2) (Obj.repr state.env);
    A.unsafe_set state.stack (state.sp + 3) (Obj.repr state.extraArgs);
    state.trapSp <- state.sp;
    state.pc <- state.pc + 1;

  | Instruct.Kpoptrap ->
    state.trapSp <- state.sp;
    state.sp <- state.sp + 4;
    state.pc <- state.pc + 1;
    
  | _ ->
    fail "unsupported opcode"

let interp state =
  let codelen = Array.length state.code in
  while state.pc < codelen do
    step state;
  done
