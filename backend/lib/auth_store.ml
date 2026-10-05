open! Core
open! Import

type t =
  { path : string
  ; mutex : Eio.Mutex.t
  }

let default_path () =
  let config_home =
    match Sys.getenv "XDG_CONFIG_HOME" with
    | Some dir when not (String.is_empty dir) -> dir
    | _ ->
      Filename.concat (Option.value (Sys.getenv "HOME") ~default:".") ".config"
  in
  Filename.concat config_home "prigh/auth.json"
;;

(* A symlink (e.g. to pi's ~/.pi/agent/auth.json) is followed so that the
   lock sits next to the real file and [save]'s rename replaces the file
   rather than the link. *)
let create ~path =
  let path =
    match Filename_unix.realpath path with
    | real -> real
    | exception Core_unix.Unix_error (ENOENT, _, _) -> path
  in
  { path; mutex = Eio.Mutex.create () }
;;

let path t = t.path

let load t : (string * Json.t) list Or_error.t =
  match Sys_unix.file_exists_exn t.path with
  | false -> Ok []
  | true ->
    let contents = In_channel.read_all t.path in
    if String.is_empty (String.strip contents)
    then Ok []
    else (
      match Json.parse contents with
      | Ok (`Object fields) -> Ok fields
      | Ok _ ->
        Or_error.error_s
          [%message "auth file must be a JSON object" ~file:(t.path : string)]
      | Error e ->
        Or_error.error_s
          [%message
            "auth file is not valid JSON" ~file:(t.path : string) (e : Error.t)])
;;

let save t fields =
  let dir = Filename.dirname t.path in
  Core_unix.mkdir_p ~perm:0o700 dir;
  let tmp = t.path ^ ".tmp" in
  let contents = Json.to_string_hum (`Object fields) ^ "\n" in
  let fd =
    Core_unix.openfile tmp ~mode:[ O_WRONLY; O_CREAT; O_TRUNC ] ~perm:0o600
  in
  Exn.protect
    ~f:(fun () ->
      ignore (Core_unix.single_write_substring fd ~buf:contents : int))
    ~finally:(fun () -> Core_unix.close fd);
  Core_unix.rename ~src:tmp ~dst:t.path
;;

(* Fibers of one process are serialised with the mutex, processes with the
   lock directory. *)
let with_lock t ~f =
  Eio.Mutex.use_rw ~protect:false t.mutex
  @@ fun () ->
  Core_unix.mkdir_p ~perm:0o700 (Filename.dirname t.path);
  Lock_dir.with_lock ~file:t.path ~f ()
;;

let parse_entry ~file (provider, json) =
  match Provider_id.of_builtin_string provider with
  | None -> None
  | Some provider ->
    (match Credential.of_json json with
     | Ok c -> Some (Ok (provider, c))
     | Error e ->
       Some
         (Or_error.error_s
            [%message
              "auth file: bad credential"
                (file : string)
                (provider : Provider_id.t)
                (e : Error.t)]))
;;

let list t =
  Or_error.bind (load t) ~f:(fun fields ->
    List.filter_map fields ~f:(parse_entry ~file:t.path)
    |> Or_error.all
    |> Or_error.map ~f:(List.sort ~compare:[%compare: Provider_id.t * _]))
;;

(* Only the provider's own entry is parsed: other tools (pi) may keep
   entries prigh cannot read under other names. *)
let read t provider =
  Or_error.bind (load t) ~f:(fun fields ->
    match List.Assoc.find fields ~equal:String.equal (Provider_id.to_string provider) with
    | None -> Ok None
    | Some json ->
      (match Credential.of_json json with
       | Ok c -> Ok (Some c)
       | Error e ->
         Or_error.error_s
           [%message
             "auth file: bad credential"
               ~file:(t.path : string)
               (provider : Provider_id.t)
               (e : Error.t)]))
;;

let mem t name =
  Or_error.map (load t) ~f:(fun fields ->
    List.Assoc.mem fields ~equal:String.equal name)
;;

let modify t provider ~f =
  with_lock t ~f:(fun () ->
    let open Or_error.Let_syntax in
    let%bind fields = load t in
    let key = Provider_id.to_string provider in
    let%bind current =
      match List.Assoc.find fields ~equal:String.equal key with
      | None -> Ok None
      | Some json -> Or_error.map (Credential.of_json json) ~f:Option.some
    in
    let%map next = f current in
    if not ([%equal: Credential.t option] current next)
    then (
      let fields = List.Assoc.remove fields ~equal:String.equal key in
      let fields =
        match next with
        | None -> fields
        | Some c -> fields @ [ key, Credential.to_json c ]
      in
      save t fields);
    next)
;;

let set t provider credential =
  Or_error.ignore_m (modify t provider ~f:(fun _ -> Ok (Some credential)))
;;

let remove t provider =
  Or_error.ignore_m (modify t provider ~f:(fun _ -> Ok None))
;;
