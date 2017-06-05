
let char_size = 8
let int_chars = (Sys.int_size + (char_size - 1)) / char_size

let int_array_of_string str =
  let length = String.length str in
  let size = (length + (int_chars - 1)) / int_chars in
  let array = Array.make size 0 in
  for i = 0 to size - 1 do
    let int = ref 0 in
    for j = 0 to int_chars - 1 do
      let index = i * int_chars + j in
      if index < length then begin
        let code = Char.code str.[index] in
        let shift = j * char_size in
        int := !int lor (code lsl shift);
      end;
    done;
    array.(i) <- !int;
  done;
  array

let request state length =
  Random.State.int state length = 0

let stop state target length =
  Random.State.int state (length * (target - 1)) = 0

let random_step state jump_freq length last =
  if Random.State.int state (length * jump_freq) = 0 then begin
    Random.State.int state length
  end else if Random.State.bool state then begin
    let next = last + 1 in
    if next < length then next
    else length - 1
  end else begin
    let next = last - 1 in
    if next >= 0 then next
    else 0
  end

let generate seed locations jumps target =
  let state = Random.State.make seed in
  let length = Array.length locations in
  if length = 0 then []
  else begin
    let jump_freq = (target - 1) / jumps in
    let first = Random.State.int state length in
    let rec loop last acc =
      let next = random_step state jump_freq length last in
      let acc =
        if request state length then locations.(next) :: acc
        else acc
      in
      if stop state target length then acc
      else loop next acc
    in
    loop first [locations.(first)]
  end

let print_loc ppf loc =
  let open Location in
  let open Lexing in
  let pos = loc.loc_start in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Format.fprintf ppf "%i:%i" line col

let enumerate ast =
  let open Ast_iterator in
  let expressions = ref [] in
  let expr it exp =
    let loc = exp.Parsetree.pexp_loc in
    expressions := loc :: !expressions;
    default_iterator.expr it exp
  in
  let enumerator = { default_iterator with expr } in
  enumerator.structure enumerator ast;
  Array.of_list !expressions

let parse_impl sourcefile =
  let ic = open_in sourcefile in
  try
    let contents =
      let buffer = Buffer.create 1024 in
      try
        while true do
          Buffer.add_channel buffer ic 1024
        done;
        assert false
      with End_of_file -> Buffer.contents buffer
    in
    let trace = Trace.null in
    let query = Mconfig.({ initial.query with filename = sourcefile }) in
    let config = Mconfig.({ initial with query }) in
    let source = Msource.make trace config contents in
    match Mreader.parse trace config source with
    | { Mreader.lexer_errors = [];
        parser_errors = [];
        parsetree = `Implementation ast } ->
      close_in ic;
      Some ast
    | { Mreader.parsetree = `Implementation _ } ->
      close_in ic;
      None
    | _ ->
      failwith "Not a valid ml file"
  with x -> close_in ic; raise x

let stop_server merlin =
  let command = merlin ^ " server stop-server" in
  match Sys.command command with
  | 255 -> ()
  | code -> failwith ("merlin exited with code " ^ string_of_int code)

let query_type merlin sourcefile location =
  let key =
    Format.asprintf "%s:%a"
      sourcefile print_loc location
  in
  let command =
    Format.asprintf
      "%s server type-enclosing -position '%a' -index 0 -filename %s < %s"
      merlin print_loc location sourcefile sourcefile
  in
  let result =
    let ic = Unix.open_process_in command in
    match Yojson.Basic.from_channel ic with
    | json -> begin
        match Unix.close_process_in ic with
        | Unix.WEXITED 0 -> json
        | Unix.WEXITED code ->
            failwith ("merlin exited with code " ^ string_of_int code)
        | _ ->
            failwith "merlin closed unexpectedly"
      end
    | exception e ->
        ignore (Unix.close_process_in ic);
        raise e
  in
    match result with
    | `Assoc alist -> begin
        match List.assoc "class" alist with
        | `String "return" -> begin
            match List.assoc "timing" alist with
            | `Assoc timings -> begin
                match List.assoc "total" timings with
                | `Float timing ->
                    Some (key, timing)
                | _ -> failwith "merlin gave bad output"
              end
            | _ ->
                failwith "merlin gave bad output"
            | exception Not_found ->
                failwith "merlin gave bad output"
          end
        | `String _ -> None
        | _ ->
            failwith "merlin gave bad output"
        | exception Not_found ->
            failwith "merlin gave bad output"
      end
    | _ ->
        failwith "merlin gave bad output"

let query_types merlin sourcefile locations =
  let rec loop acc locations =
    match locations with
    | [] -> List.rev acc
    | location :: rest ->
      let acc =
        match query_type merlin sourcefile location with
        | None -> acc
        | Some timing -> timing :: acc
      in
      loop acc rest
  in
  loop [] locations

let set_merlin_socket () =
  let socket = Filename.temp_file "merlin_" ".socket" in
  Unix.putenv "MERLIN_SOCKET" socket

let print_result ppf (key, timing) =
  Format.fprintf ppf "%s %a"
    key Format.pp_print_float timing

let print_results ppf results =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    print_result ppf results

let usage =
  "benchmerlin MERLIN FILE"

let () =
  let args = ref [] in
  Arg.parse []
    (fun arg -> args := arg :: !args)
    usage;
  let merlin, file =
    match !args with
    | [file; merlin] -> merlin, file
    | _ ->
      Arg.usage [] usage;
      exit 1
  in
  let jumps = 1 in
  let target = 10 in
  match parse_impl file with
  | None -> ()
  | Some ast ->
    let locations = enumerate ast in
    let seed = int_array_of_string file in
    let session = generate seed locations jumps target in
    set_merlin_socket ();
    let results = query_types merlin file session in
    stop_server merlin;
    Format.printf "%a\n%!" print_results results
