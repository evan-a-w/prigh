open! Core
open! Import

let list_op = "$fs_list"
let stat_op = "$fs_stat"
let read_op = "$fs_read"
let mkdir_op = "$fs_mkdir"
let rename_op = "$fs_rename"
let delete_op = "$fs_delete"
let download_op = "$fs_download"
let upload_op = "$fs_upload"

let is_op name =
  List.mem
    [ list_op
    ; stat_op
    ; read_op
    ; mkdir_op
    ; rename_op
    ; delete_op
    ; download_op
    ; upload_op
    ]
    name
    ~equal:String.equal
;;

let max_entries = 5000
let max_read = 1024 * 1024
let chunk_size = 3 * 64 * 1024

exception Failed of string

let fail fmt = ksprintf (fun s -> raise (Failed s)) fmt

let check_name name =
  if String.is_empty name
  then Or_error.error_string "the name is empty"
  else if String.equal name "." || String.equal name ".."
  then Or_error.errorf "%S is not a name" name
  else if String.mem name '/'
  then Or_error.errorf "%S: a name cannot contain /" name
  else if String.mem name '\000'
  then Or_error.error_string "a name cannot contain NUL"
  else Ok ()
;;

let home () = Option.value (Sys.getenv "HOME") ~default:"/"

(* Absolute, without [.], [..], empty segments or a trailing [/]: a
   trailing [/] would make [link/] mean the directory [link] points to. *)
let normalize path =
  let parts =
    List.fold (String.split path ~on:'/') ~init:[] ~f:(fun acc part ->
      match part with
      | "" | "." -> acc
      | ".." -> List.tl acc |> Option.value ~default:[]
      | p -> p :: acc)
  in
  "/" ^ String.concat ~sep:"/" (List.rev parts)
;;

let path_arg ~cwd args key =
  let path = Tool_args.string args key in
  if String.is_empty (String.strip path) then fail "%s is empty" key;
  if String.mem path '\000' then fail "%s contains NUL" key;
  normalize (Tool.resolve ~cwd path)
;;

(* A path whose last segment, as written, names something to create. *)
let target_arg ~cwd args key =
  let path = path_arg ~cwd args key in
  let written =
    Filename.basename
      (String.rstrip (Tool_args.string args key) ~drop:(Char.equal '/'))
  in
  (match check_name written with
   | Ok () -> ()
   | Error e -> fail "%s" (Error.to_string_hum e));
  path
;;

let errno_text path exn =
  match exn with
  | Core_unix.Unix_error (ENOENT, _, _) -> sprintf "%s does not exist" path
  | Core_unix.Unix_error (EACCES, _, _) | Core_unix.Unix_error (EPERM, _, _) ->
    sprintf "permission denied: %s" path
  | Core_unix.Unix_error (EEXIST, _, _) -> sprintf "%s already exists" path
  | Core_unix.Unix_error (ENOTDIR, _, _) -> sprintf "not a directory: %s" path
  | Core_unix.Unix_error (ENOTEMPTY, _, _) -> sprintf "%s is not empty" path
  | Core_unix.Unix_error (ENOSPC, _, _) -> sprintf "no space left for %s" path
  | Core_unix.Unix_error (EXDEV, _, _) ->
    sprintf "%s: cannot move across filesystems" path
  | Core_unix.Unix_error (err, _, _) ->
    sprintf "%s: %s" path (Core_unix.Error.message err)
  | Sys_error message -> message
  | exn -> sprintf "%s: %s" path (Exn.to_string exn)
;;

(* Runs [f], turning a Unix error about [path] into a message. *)
let unix path f =
  match f () with
  | x -> x
  | exception (Failed _ as exn) -> raise exn
  | exception exn -> raise (Failed (errno_text path exn))
;;

let kind_name : Core_unix.file_kind -> string = function
  | S_REG -> "file"
  | S_DIR -> "dir"
  | S_LNK -> "link"
  | S_CHR | S_BLK | S_FIFO | S_SOCK -> "other"
;;

let mtime_ms (st : Core_unix.stats) =
  `Number (Int.to_string (Float.iround_down_exn (st.st_mtime *. 1000.)))
;;

let number n = `Number (Int.to_string n)
let size (st : Core_unix.stats) = number (Int64.to_int_exn st.st_size)

let is_dir path =
  match Core_unix.stat path with
  | st -> Poly.equal st.st_kind S_DIR
  | exception _ -> false
;;

let entry ~dir name =
  let path = Filename.concat dir name in
  match Core_unix.lstat path with
  | exception _ -> None
  | st ->
    let target =
      match st.st_kind with
      | S_LNK ->
        [ ( "target"
          , `String
              (match Core_unix.stat path with
               | target -> kind_name target.st_kind
               | exception _ -> "missing") )
        ]
      | _ -> []
    in
    (* Only files have a size worth showing. *)
    let size =
      match st.st_kind with
      | S_REG -> [ "size", size st ]
      | S_LNK ->
        (match Core_unix.stat path with
         | { st_kind = S_REG; _ } as target -> [ "size", size target ]
         | _ | (exception _) -> [])
      | S_DIR | S_CHR | S_BLK | S_FIFO | S_SOCK -> []
    in
    Some
      (`Object
          ([ "name", `String name; "kind", `String (kind_name st.st_kind) ]
           @ size
           @ [ "mtime", mtime_ms st ]
           @ target))
;;

let parent path =
  if String.equal path "/" then `Null else `String (Filename.dirname path)
;;

let list ~cwd args =
  let path = path_arg ~cwd args "path" in
  let hidden = Option.value (Tool_args.bool_opt args "hidden") ~default:false in
  let real = unix path (fun () -> Filename_unix.realpath path) in
  let dir, select =
    if is_dir real
    then real, None
    else Filename.dirname real, Some (Filename.basename real)
  in
  let names = unix dir (fun () -> Sys_unix.readdir dir) |> Array.to_list in
  let shown, dots =
    List.partition_tf names ~f:(fun name ->
      hidden || not (String.is_prefix name ~prefix:"."))
  in
  let shown = List.sort shown ~compare:String.compare in
  let entries = List.filter_map (List.take shown max_entries) ~f:(entry ~dir) in
  `Object
    ([ "path", `String dir
     ; "parent", parent dir
     ; "home", `String (home ())
     ; "entries", `Array entries
     ; "total", number (List.length shown)
     ; "hidden", number (if hidden then 0 else List.length dots)
     ]
     @ Option.value_map select ~default:[] ~f:(fun name ->
       [ "select", `String name ]))
;;

let stat ~cwd args =
  let path = path_arg ~cwd args "path" in
  let st = unix path (fun () -> Core_unix.stat path) in
  `Object
    [ "path", `String path
    ; "kind", `String (kind_name st.st_kind)
    ; "size", size st
    ; "mtime", mtime_ms st
    ]
;;

(* [In_channel.input] until [len] bytes or the end. *)
let input_at_most ic ~buf ~pos ~len =
  let rec go got =
    if got = len
    then got
    else (
      match In_channel.input ic ~buf ~pos:(pos + got) ~len:(len - got) with
      | 0 -> got
      | n -> go (got + n))
  in
  go 0
;;

(* Opening a FIFO blocks and a device may never end. *)
let check_readable path (st : Core_unix.stats) =
  match st.st_kind with
  | S_REG -> ()
  | S_DIR -> fail "%s is a directory" path
  | S_CHR | S_BLK | S_FIFO | S_SOCK | S_LNK ->
    fail "%s is not a regular file: read it with a command instead" path
;;

let read ~cwd args =
  let path = path_arg ~cwd args "path" in
  let offset = Option.value (Tool_args.int_opt args "offset") ~default:0 in
  let length =
    Int.clamp_exn
      (Option.value (Tool_args.int_opt args "length") ~default:max_read)
      ~min:0
      ~max:max_read
  in
  if offset < 0 then fail "offset must be >= 0";
  let st = unix path (fun () -> Core_unix.stat path) in
  check_readable path st;
  let data =
    unix path (fun () ->
      In_channel.with_file path ~binary:true ~f:(fun ic ->
        In_channel.seek ic (Int64.of_int offset);
        let buf = Bytes.create length in
        let n = input_at_most ic ~buf ~pos:0 ~len:length in
        Bytes.To_string.sub buf ~pos:0 ~len:n))
  in
  let size = Int64.to_int_exn st.st_size in
  `Object
    [ "size", number size
    ; "mtime", mtime_ms st
    ; "data", `String (Base64.encode_string data)
    ; ("eof", if offset + String.length data >= size then `True else `False)
    ]
;;

let mkdir ~cwd args =
  let path = target_arg ~cwd args "path" in
  let parents =
    Option.value (Tool_args.bool_opt args "parents") ~default:false
  in
  let ignore_all =
    Option.value (Tool_args.bool_opt args "ignore_all") ~default:false
  in
  let existed = is_dir path in
  if parents
  then unix path (fun () -> Core_unix.mkdir_p path)
  else unix path (fun () -> Core_unix.mkdir path);
  if ignore_all && not existed
  then
    unix path (fun () ->
      Out_channel.write_all (Filename.concat path ".gitignore") ~data:"*\n");
  `Object [ "path", `String path ]
;;

let exists path =
  match Core_unix.lstat path with
  | _ -> true
  | exception _ -> false
;;

let rename ~cwd args =
  let from = path_arg ~cwd args "from" in
  let to_ = target_arg ~cwd args "to" in
  let overwrite =
    Option.value (Tool_args.bool_opt args "overwrite") ~default:false
  in
  if not (exists from) then fail "%s does not exist" from;
  if (not overwrite) && exists to_ && not (String.equal from to_)
  then fail "%s already exists" to_;
  unix to_ (fun () -> Core_unix.rename ~src:from ~dst:to_);
  `Object [ "path", `String to_ ]
;;

let rec remove path =
  match Core_unix.lstat path with
  | { st_kind = S_DIR; _ } ->
    Array.iter (Sys_unix.readdir path) ~f:(fun name ->
      remove (Filename.concat path name));
    Core_unix.rmdir path
  | _ -> Core_unix.unlink path
;;

(* [path] with its directory's symlinks resolved; itself if that fails. *)
let real_parent path =
  if String.equal path "/"
  then path
  else (
    match Filename_unix.realpath (Filename.dirname path) with
    | dir -> Filename.concat dir (Filename.basename path)
    | exception _ -> path)
;;

let delete ~cwd args =
  let path = path_arg ~cwd args "path" in
  let home = normalize (home ()) in
  let real_home =
    match Filename_unix.realpath home with
    | real -> real
    | exception _ -> home
  in
  let holds_home p =
    String.equal p "/"
    || List.exists [ home; real_home ] ~f:(fun home ->
      String.equal p home || String.is_prefix home ~prefix:(p ^ "/"))
  in
  if holds_home path || holds_home (real_parent path)
  then fail "refusing to delete %s" path;
  if not (exists path) then fail "%s does not exist" path;
  unix path (fun () -> remove path);
  `Object [ "path", `String path ]
;;

(* Output that may run [window] chunks ahead of the credits granted on
   [stdin] (one per [+]). [send] is false once the reader is gone. *)
module Credits = struct
  type t =
    { feed : Stdin_feed.t
    ; cancel : Cancellation.t
    ; on_output : string -> unit
    ; mutable credits : int
    ; mutable closed : bool
    ; mutable bytes : int
    }

  let rec await t =
    if Cancellation.is_cancelled t.cancel
    then false
    else if t.credits > 0
    then true
    else if t.closed
    then false
    else (
      match
        Cancellation.protect t.cancel ~f:(fun () -> Stdin_feed.take t.feed)
      with
      | None -> false
      | Some (Data data) ->
        t.credits <- t.credits + String.count data ~f:(Char.equal '+');
        await t
      | Some Close ->
        t.closed <- true;
        false)
  ;;

  let send t chunk =
    String.is_empty chunk
    || (await t
        &&
        (t.credits <- t.credits - 1;
         t.bytes <- t.bytes + String.length chunk;
         t.on_output (Base64.encode_string chunk);
         true))
  ;;
end

let download ~env ~stdin ~cancel ~on_output ~cwd args =
  let path = path_arg ~cwd args "path" in
  let archive =
    Option.value (Tool_args.bool_opt args "archive") ~default:false
  in
  let window =
    Int.clamp_exn
      (Option.value (Tool_args.int_opt args "window") ~default:16)
      ~min:1
      ~max:256
  in
  let feed =
    match stdin with
    | Some feed -> feed
    | None -> fail "%s needs a stdin feed for its credits" download_op
  in
  let credits =
    { Credits.feed
    ; cancel
    ; on_output
    ; credits = window
    ; closed = false
    ; bytes = 0
    }
  in
  let st = unix path (fun () -> Core_unix.stat path) in
  (match archive, st.st_kind with
   | true, S_DIR when String.equal path "/" -> fail "refusing to archive /"
   | true, S_DIR -> ()
   | true, _ -> fail "not a directory: %s" path
   | false, S_DIR -> fail "%s is a directory: download it as an archive" path
   | false, _ -> check_readable path st);
  if archive
  then (
    let stopped = Cancellation.create () in
    let stderr = Buffer.create 256 in
    let exit =
      Switch.run (fun sw ->
        Fiber.fork_daemon ~sw (fun () ->
          Cancellation.await cancel;
          Cancellation.cancel stopped;
          `Stop_daemon);
        Process.run
          ~env
          ~cancel:stopped
          ~on_stdout:(fun chunk ->
            if not (Credits.send credits chunk) then Cancellation.cancel stopped)
          ~on_stderr:(fun s ->
            if Buffer.length stderr < 4096 then Buffer.add_string stderr s)
          ~prog:"tar"
          ~args:
            [ "-czf"
            ; "-"
            ; "-C"
            ; Filename.dirname path
            ; "--"
            ; Filename.basename path
            ]
          ())
    in
    match exit with
    | Exited 0 -> ()
    | Exited 1 ->
      (* GNU tar: some files changed while they were read *)
      ()
    | Cancelled -> fail "[cancelled]"
    | Exited n ->
      fail "tar exited with %d: %s" n (String.strip (Buffer.contents stderr))
    | Signaled _ | Timed_out -> fail "tar was killed")
  else
    unix path (fun () ->
      In_channel.with_file path ~binary:true ~f:(fun ic ->
        let buf = Bytes.create chunk_size in
        let rec loop () =
          let n = input_at_most ic ~buf ~pos:0 ~len:chunk_size in
          if n > 0
          then
            if Credits.send credits (Bytes.To_string.sub buf ~pos:0 ~len:n)
            then loop ()
            else fail "[cancelled]"
        in
        loop ()));
  `Object [ "bytes", number credits.bytes ]
;;

(* [name-2.ext], [name-3.ext], ... next to [path]: the first that is free. *)
let free_name path =
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  let stem, ext =
    match String.rsplit2 base ~on:'.' with
    | Some (stem, ext) when not (String.is_empty stem) -> stem, "." ^ ext
    | _ -> base, ""
  in
  let rec go n =
    let candidate = Filename.concat dir (sprintf "%s-%d%s" stem n ext) in
    if exists candidate then go (n + 1) else candidate
  in
  if exists path then go 2 else path
;;

let temp_seq = ref 0

let next_temp () =
  incr temp_seq;
  !temp_seq
;;

let upload ~stdin ~cancel ~on_output ~cwd args =
  let path = target_arg ~cwd args "path" in
  let conflict =
    Option.value (Tool_args.string_opt args "conflict") ~default:"fail"
  in
  let size = Tool_args.int_opt args "size" in
  (match conflict with
   | "fail" | "overwrite" | "rename" -> ()
   | other -> fail "unknown conflict %S: fail, overwrite or rename" other);
  let feed =
    match stdin with
    | Some feed -> feed
    | None -> fail "%s needs a stdin feed for the data" upload_op
  in
  let dir = Filename.dirname path in
  if not (is_dir dir) then fail "not a directory: %s" dir;
  if String.equal conflict "fail" && exists path
  then fail "%s already exists" path;
  let temp =
    Filename.concat
      dir
      (sprintf
         ".%s.prigh-upload-%d-%d"
         (* within NAME_MAX (255 bytes) *)
         (String.prefix (Filename.basename path) 200)
         (Core_unix.getpid () |> Pid.to_int)
         (next_temp ()))
  in
  let oc =
    unix temp (fun () ->
      Out_channel.create ~binary:true ~fail_if_exists:true temp)
  in
  let bytes = ref 0 in
  let cleanup () =
    Out_channel.close_no_err oc;
    try Core_unix.unlink temp with
    | _ -> ()
  in
  let rec loop () =
    match Cancellation.protect cancel ~f:(fun () -> Stdin_feed.take feed) with
    | None -> `Cancelled
    | Some Close -> `Done
    | Some (Data data) ->
      (match Base64.decode data with
       | Error (`Msg m) -> `Failed ("bad data: " ^ m)
       | Ok raw ->
         (match Out_channel.output_string oc raw with
          | () ->
            bytes := !bytes + String.length raw;
            on_output "+";
            loop ()
          | exception exn -> `Failed (errno_text path exn)))
  in
  match loop () with
  | `Cancelled ->
    cleanup ();
    fail "[cancelled]"
  | `Failed message ->
    cleanup ();
    fail "%s" message
  | `Done ->
    (match Out_channel.close oc with
     | () -> ()
     | exception exn ->
       cleanup ();
       fail "%s" (errno_text path exn));
    (match size with
     | Some size when size <> !bytes ->
       cleanup ();
       fail "%s: got %d of %d bytes" path !bytes size
     | Some _ | None -> ());
    let target =
      match conflict with
      | "rename" -> free_name path
      | "fail" when exists path ->
        cleanup ();
        fail "%s already exists" path
      | _ -> path
    in
    (match Core_unix.rename ~src:temp ~dst:target with
     | () -> ()
     | exception exn ->
       cleanup ();
       fail "%s" (errno_text target exn));
    `Object [ "path", `String target; "bytes", number !bytes ]
;;

let execute ~env ?stdin ~cancel ~on_output ~cwd ~name ~arguments () =
  match
    if String.equal name list_op
    then list ~cwd arguments
    else if String.equal name stat_op
    then stat ~cwd arguments
    else if String.equal name read_op
    then read ~cwd arguments
    else if String.equal name mkdir_op
    then mkdir ~cwd arguments
    else if String.equal name rename_op
    then rename ~cwd arguments
    else if String.equal name delete_op
    then delete ~cwd arguments
    else if String.equal name download_op
    then download ~env ~stdin ~cancel ~on_output ~cwd arguments
    else if String.equal name upload_op
    then upload ~stdin ~cancel ~on_output ~cwd arguments
    else fail "unknown host tool %S" name
  with
  | json -> Tool.Result.ok (Json.to_string json)
  | exception Failed message -> Tool.Result.error message
  | exception Tool_args.Invalid message -> Tool.Result.error message
;;
