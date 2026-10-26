open! Core
open! Import

type t =
  { check : unit -> unit Or_error.t
  ; store : unit -> Nix_store_status.Store.t Or_error.t
  ; last_gc : unit -> Nix_gc_report.t option
  ; running : unit -> Nix_store_status.Running.t option
  ; schedule : unit -> Nix_store_status.Schedule.t option
  ; collect :
      by:string
      -> Nix_generations.t option
      -> on_line:(string -> unit)
      -> Nix_gc_report.t Or_error.t
  }

let words line =
  String.split line ~on:' '
  |> List.concat_map ~f:(String.split ~on:'\t')
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let parse_path_info output =
  List.fold_result
    (String.split_lines output)
    ~init:(0, 0)
    ~f:(fun (paths, bytes) line ->
      match words line with
      | [] -> Ok (paths, bytes)
      | [ _path; size ] ->
        (match Int.of_string_opt size with
         | Some size -> Ok (paths + 1, bytes + size)
         | None -> Or_error.errorf "unexpected nix path-info line %S" line)
      | _ -> Or_error.errorf "unexpected nix path-info line %S" line)
;;

let parse_df output =
  match String.split_lines output with
  | _header :: line :: _ ->
    (match words line with
     | _fs :: total :: _used :: available :: _ ->
       (match Int.of_string_opt total, Int.of_string_opt available with
        | Some total, Some available -> Ok (total * 1024, available * 1024)
        | _ -> Or_error.errorf "unexpected df line %S" line)
     | _ -> Or_error.errorf "unexpected df line %S" line)
  | _ -> Or_error.errorf "unexpected df output %S" output
;;

let no_nix =
  "nix is not installed where the backend runs (no nix on its PATH), so there \
   is no Nix store to clean"
;;

let check_nix () =
  if Host_ops.on_path "nix" then Ok () else Or_error.error_string no_nix
;;

let describe_exit : Process.Exit.t -> string = function
  | Exited code -> sprintf "exit code %d" code
  | Signaled signal -> sprintf "killed by %s" (Signal.to_string signal)
  | Timed_out -> "timed out"
  | Cancelled -> "cancelled"
;;

let run ~env prog args =
  match
    Process.run_collect ~env ~extra_env:[ "LC_ALL", "C" ] ~prog ~args ()
  with
  | { exit = Exited 0; stdout; _ } -> Ok stdout
  | { exit; stderr; _ } ->
    Or_error.errorf
      "%s %s failed (%s): %s"
      prog
      (String.concat ~sep:" " args)
      (describe_exit exit)
      (String.strip stderr)
  | exception Eio.Io _ -> Or_error.errorf "cannot run %s: is it installed?" prog
;;

let store_dir = "/nix/store"

let free_space ~env =
  Or_error.bind (run ~env "df" [ "-Pk"; store_dir ]) ~f:parse_df
;;

let store ~env () =
  let open Or_error.Let_syntax in
  let%bind () = check_nix () in
  let%bind paths, bytes =
    run ~env "nix" [ "path-info"; "--all"; "-s" ] >>= parse_path_info
  in
  let%map fs_total_bytes, fs_free_bytes = free_space ~env in
  { Nix_store_status.Store.bytes; paths; fs_total_bytes; fs_free_bytes }
;;

let now ~env = Eio.Time.now (Eio.Stdenv.clock env)

(* The last few lines, to say why a command failed. *)
let tail lines = List.take !lines 5 |> List.rev |> String.concat ~sep:"\n"

let local ~env =
  let collect ~by generations ~on_line =
    let open Or_error.Let_syntax in
    let%bind () = check_nix () in
    let started = now ~env in
    let free_before = Result.ok (free_space ~env) in
    let tally = Nix_gc_output.Tally.create () in
    let recent = ref [] in
    let splitter =
      Nix_gc_output.Splitter.create (fun line ->
        recent := line :: List.take !recent 4;
        Nix_gc_output.Tally.add tally (Nix_gc_output.Line.classify line);
        on_line line)
    in
    let prog, args =
      match (generations : Nix_generations.t option) with
      | None -> "nix", [ "store"; "gc"; "-v" ]
      | Some All -> "nix-collect-garbage", [ "-v"; "-d" ]
      | Some (Older_than_days days) ->
        ( "nix-collect-garbage"
        , [ "-v"; "--delete-older-than"; sprintf "%dd" days ] )
    in
    let exit =
      match
        Process.run
          ~env
          ~extra_env:[ "LC_ALL", "C" ]
          ~on_stdout:(Nix_gc_output.Splitter.feed splitter)
          ~on_stderr:(Nix_gc_output.Splitter.feed splitter)
          ~prog
          ~args
          ()
      with
      | exit -> Ok exit
      | exception Eio.Io _ ->
        Or_error.errorf "cannot run %s: is it installed?" prog
    in
    Nix_gc_output.Splitter.flush splitter;
    match%bind exit with
    | Exited 0 ->
      let finished = now ~env in
      let freed_bytes =
        match tally.freed_bytes, free_before, Result.ok (free_space ~env) with
        | Some bytes, _, _ -> bytes
        | None, Some (_, before), Some (_, after) -> Int.max 0 (after - before)
        | None, _, _ -> 0
      in
      Ok
        { Nix_gc_report.at = finished
        ; by
        ; generations
        ; generations_deleted = tally.generations_deleted
        ; paths_deleted = tally.paths_deleted
        ; freed_bytes
        ; seconds = finished -. started
        }
    | exit ->
      Or_error.errorf
        "%s failed (%s): %s"
        prog
        (describe_exit exit)
        (tail recent)
  in
  { check = check_nix
  ; store = store ~env
  ; last_gc = (fun () -> None)
  ; running = (fun () -> None)
  ; schedule = (fun () -> None)
  ; collect
  }
;;

let read_file path =
  match In_channel.read_all path with
  | text -> Some text
  | exception Sys_error _ -> None
;;

(* Bytes of [path] from [offset] on. *)
let read_from path ~offset =
  match
    In_channel.with_file path ~f:(fun ic ->
      In_channel.seek ic (Int64.of_int offset);
      In_channel.input_all ic)
  with
  | text -> text
  | exception Sys_error _ -> ""
;;

let fields_file path ~f =
  Option.bind (read_file path) ~f:(fun text -> Result.ok (f text))
;;

let request_seq = ref 0

(* Unique among this helper's requests: the helper serves one backend. *)
let request_id () =
  incr request_seq;
  sprintf
    "%x%x%x"
    (Pid.to_int (Core_unix.getpid ()))
    (Int63.to_int_exn (Time_ns.to_int63_ns_since_epoch (Time_ns.now ()))
     land 0xffffffff)
    !request_seq
;;

(* O_NONBLOCK: with nobody reading the FIFO, opening it fails instead of
   waiting. *)
let send_request ~fifo line =
  match Core_unix.openfile fifo ~mode:[ O_WRONLY; O_NONBLOCK ] with
  | exception Core_unix.Unix_error (ENXIO, _, _) ->
    Or_error.error_string
      "the container's Nix garbage collector is not running: restart the \
       container"
  | exception Core_unix.Unix_error (e, _, _) ->
    Or_error.errorf
      "cannot reach the Nix garbage collector (%s): %s"
      fifo
      (Core_unix.Error.message e)
  | fd ->
    Exn.protect
      ~finally:(fun () -> Core_unix.close fd)
      ~f:(fun () ->
        let written = Core_unix.single_write_substring fd ~buf:line in
        if written = String.length line
        then Ok ()
        else
          Or_error.error_string
            "the Nix garbage collector's request was cut short")
;;

let helper ~env ~dir =
  let fifo = Filename.concat dir "requests" in
  let file name = Filename.concat dir name in
  let running () =
    fields_file (file "running") ~f:Nix_store_status.Running.of_fields
  in
  let check () =
    Or_error.bind (check_nix ()) ~f:(fun () ->
      match Core_unix.stat fifo with
      | { st_kind = S_FIFO; _ } -> Ok ()
      | _ | (exception Core_unix.Unix_error _) ->
        Or_error.errorf
          "the container's Nix garbage collector is not set up (no %s): \
           restart the container"
          fifo)
  in
  let collect ~by generations ~on_line =
    let open Or_error.Let_syntax in
    let%bind () = check () in
    let id = request_id () in
    let%bind () =
      send_request
        ~fifo
        (sprintf "gc %s %s %s\n" id (Nix_generations.to_arg generations) by)
    in
    let log = file (id ^ ".log") in
    let result = file (id ^ ".result") in
    let splitter = Nix_gc_output.Splitter.create on_line in
    let offset = ref 0 in
    let drain () =
      let text = read_from log ~offset:!offset in
      offset := !offset + String.length text;
      Nix_gc_output.Splitter.feed splitter text
    in
    let clock = Eio.Stdenv.clock env in
    (* Only time waiting for an idle helper to start counts. *)
    let rec wait ~idle =
      drain ();
      match read_file result with
      | Some text ->
        drain ();
        Nix_gc_output.Splitter.flush splitter;
        let fields = Nix_gc_report.fields text in
        (match List.Assoc.find fields ~equal:String.equal "status" with
         | Some "ok" ->
           Or_error.tag
             (Nix_gc_report.of_fields text)
             ~tag:"the Nix garbage collector's report"
         | _ ->
           Or_error.error_string
             (Option.value
                (List.Assoc.find fields ~equal:String.equal "error")
                ~default:"the Nix garbage collection failed"))
      | None ->
        let started = !offset > 0 || Option.is_some (running ()) in
        if (not started) && Float.(idle > 30.)
        then
          Or_error.error_string
            "the container's Nix garbage collector did not take the request: \
             see the container's log"
        else (
          Eio.Time.sleep clock 0.2;
          wait ~idle:(if started then 0. else idle +. 0.2))
    in
    wait ~idle:0.
  in
  { check
  ; store = (fun () -> Or_error.bind (check ()) ~f:(store ~env))
  ; last_gc =
      (fun () -> fields_file (file "last-gc") ~f:Nix_gc_report.of_fields)
  ; running
  ; schedule =
      (fun () ->
        fields_file (file "schedule") ~f:Nix_store_status.Schedule.of_fields)
  ; collect
  }
;;
