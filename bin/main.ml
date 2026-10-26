open Stepper

let source = ref None
let file = ref None
let mode = ref "step"
let limit = ref 10000

let options =
  [
    ( "--expr",
      Arg.String (fun s -> source := Some s),
      "PROGRAM Source expression" );
    ( "--mode",
      Arg.Symbol ([ "step"; "decompose"; "trace" ], fun s -> mode := s),
      "MODE step (default), decompose, or trace (choose leftmost)" );
    ("--limit", Arg.Set_int limit, "N Maximum transitions (default 10000)");
  ]

let read channel =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line channel);
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  Buffer.contents buffer

let print_obj i (obj : Step.obj) =
  Printf.printf "  [%d] %s\n      in %s\n" i (Print.exp obj.exp)
    (Print.ctx obj.ctx)

let trace exp =
  let rec go remaining count exp =
    match Step.decompose exp with
    | [] -> Printf.printf "value: %s\n" (Print.exp exp)
    | objs ->
        if remaining = 0 then raise (Step.Limit exp);
        let chosen =
          match List.find_opt (fun (o : Step.act_obj) -> o.act = Eval) objs with
          | Some obj -> obj
          | None -> List.hd objs
        in
        let next = Ctx.compose chosen.ctx (Step.transition chosen.exp) in
        Printf.printf "%d %s (level %d): %s\n  => %s\n" count
          (if chosen.act = Eval then "silent" else "visible")
          chosen.act_lvl (Print.exp chosen.exp) (Print.exp next);
        go (remaining - 1) (count + 1) next
  in
  go !limit 1 exp

let main () =
  Arg.parse options
    (fun name ->
      if Option.is_some !file then raise (Arg.Bad "Use only one input file");
      file := Some name)
    "stepper [--expr PROGRAM | FILE] [--mode MODE] [--limit N]";
  if !limit < 0 then invalid_arg "--limit must be nonnegative";
  let source =
    match (!source, !file) with
    | Some _, Some _ -> invalid_arg "Use either --expr or FILE"
    | Some s, None -> s
    | None, Some path ->
        let channel = open_in path in
        Fun.protect
          ~finally:(fun () -> close_in channel)
          (fun () -> read channel)
    | None, None -> read stdin
  in
  match Parse.parse source with
  | None -> invalid_arg "Empty input"
  | Some exp -> (
      match !mode with
      | "decompose" ->
          let objs = Step.decompose exp in
          if objs = [] then Printf.printf "value: %s\n" (Print.exp exp);
          List.iteri
            (fun i (o : Step.act_obj) ->
              Printf.printf "[%d] %s, action level %d\n" i
                (if o.act = Eval then "eval" else "pause")
                o.act_lvl;
              print_obj i { ctx = o.ctx; exp = o.exp })
            objs
      | "trace" -> trace exp
      | _ -> (
          match Step.step ~limit:!limit exp with
          | Val e -> Printf.printf "value: %s\n" (Print.exp e)
          | Exp objs ->
              Printf.printf "paused: %d choice(s)\n" (List.length objs);
              List.iteri print_obj objs))

let () =
  try main () with
  | Parse.Error (offset, message) ->
      Printf.eprintf "parse error at byte %d: %s\n" offset message;
      exit 2
  | Step.Unbound_variable exp ->
      Printf.eprintf "unbound variable: %s\n" (Print.exp exp);
      exit 2
  | Step.Mismatched_type exp ->
      Printf.eprintf "mismatched type: %s\n" (Print.exp exp);
      exit 2
  | Step.Limit exp ->
      Printf.eprintf "transition limit reached: %s\n" (Print.exp exp);
      exit 3
  | Invalid_argument message | Sys_error message ->
      Printf.eprintf "%s\n" message;
      exit 2
