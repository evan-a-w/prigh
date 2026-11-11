open! Core
open! Import

module Host = struct
  type t =
    { id : string
    ; name : string
    ; home : string option
    ; active : bool
    ; computer : bool
    }
  [@@deriving sexp_of]
end

let name = "on_host"

let runs =
  [ "bash"; "read"; "write"; "edit"; "apply_patch"; "ls"; "grep"; "find" ]
;;

let example =
  {|on_host {host: "NAME", tool: "bash", args: {command: "uname -a"}}|}
;;

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; native = None
  ; destructive = Never (* the inner call is approved as itself *)
  ; description =
      "Run one tool call on another tool host; only when the user names that \
       machine or asks to run something on another one. Never routinely: your \
       other tools already run on the session's own host, which is for normal \
       work; this does not move the session. Without host: list the hosts."
  ; parameters =
      Tool_args.schema
        [ "host", `String, "A connected tool host, by name or id"
        ; ( "tool"
          , `String
          , "With host: bash, read, write, edit (or apply_patch), ls, grep or \
             find" )
        ; "args", `Object, "The tool's own arguments, as for a normal call"
        ; ( "cwd"
          , `String
          , "Where relative paths and bash start on that host (default: its \
             home)" )
        ]
  }
;;

let inner args =
  match
    ( Tool_args.string_opt args "host"
    , Tool_args.string_opt args "tool"
    , Json.member "args" args )
  with
  | Some host, Some tool, Some (`Object _ as args) -> Some (host, tool, args)
  | _ | (exception Tool_args.Invalid _) -> None
;;

let describe (h : Host.t) =
  sprintf
    "- %s (id %s): home %s%s%s"
    h.name
    h.id
    (Option.value h.home ~default:"unknown")
    (if h.active
     then ", the session's active host (call tools directly)"
     else "")
    (if h.computer then ", has a computer" else "")
;;

let listing hosts =
  match hosts with
  | [] -> "no tool host is connected"
  | hosts ->
    String.concat_lines
      (("tool hosts:" :: List.map hosts ~f:describe)
       @ [ "run a tool on another one: " ^ example ])
;;

(* The hosts closest to [key] first, by name or id. *)
let closest hosts key =
  let distance (h : Host.t) =
    Int.min
      (Edit_distance.caseless h.name key)
      (Edit_distance.caseless h.id key)
  in
  List.stable_sort hosts ~compare:(fun a b ->
    Int.compare (distance a) (distance b))
;;

let find hosts key =
  match List.find hosts ~f:(fun (h : Host.t) -> String.equal h.id key) with
  | Some host -> Ok host
  | None ->
    (match
       List.filter hosts ~f:(fun (h : Host.t) ->
         String.Caseless.equal h.name key)
     with
     | [ host ] -> Ok host
     | [] ->
       Error
         (sprintf
            "no connected tool host %S (it may have disconnected: the user can \
             connect it with `prigh tool-host -connect` or a TUI there). %s"
            key
            (match hosts with
             | [] -> "No tool host is connected."
             | hosts ->
               "Connected:\n"
               ^ String.concat_lines (List.map (closest hosts key) ~f:describe)))
     | several ->
       Error
         (sprintf
            "several tool hosts are named %S: give one's id (%s)"
            key
            (String.concat
               ~sep:", "
               (List.map several ~f:(fun (h : Host.t) -> h.id)))))
;;

let not_run tool =
  let why =
    match tool with
    | "on_host" -> "on_host cannot run itself"
    | "computer" ->
      "computer drives the session's active host only; the user switches hosts \
       with /host"
    | "subagent" | "subagent_continue" ->
      sprintf
        "%s runs in the backend, not on a tool host; a subagent can use \
         on_host itself"
        tool
    | tool when String.is_prefix tool ~prefix:"job_" ->
      "background jobs belong to the session's active host"
    | tool -> sprintf "%s does not run on a tool host" tool
  in
  sprintf "%s: on_host runs %s" why (String.concat ~sep:", " runs)
;;

let starts_job args =
  match Json.member "background" args with
  | Some `True -> true
  | _ -> false
;;

let cwd_on (host : Host.t) given =
  match given, host.home with
  | None, Some home -> Ok home
  | Some cwd, home when Filename.is_absolute (Tool.expand_home ?home cwd) ->
    Ok (Tool.resolve ?home ~cwd:"/" cwd)
  | Some cwd, Some home -> Ok (Tool.resolve ~home ~cwd:home cwd)
  | (None | Some _), None ->
    Error
      (sprintf
         "tool host %s runs an older prigh that does not say its home: give \
          cwd as an absolute path"
         host.name)
;;

let run_on (context : Tool.Context.t) (host : Host.t) ~tool ~args ~cwd =
  let mine =
    List.filter context.tools ~f:(fun t ->
      List.mem runs (Tool.name t) ~equal:String.equal)
  in
  match List.find mine ~f:(fun t -> String.equal (Tool.name t) tool) with
  | None when List.mem runs tool ~equal:String.equal ->
    Tool.Result.error
      (sprintf
         "%s is not one of your tools: on_host runs %s"
         tool
         (String.concat ~sep:", " (List.map mine ~f:Tool.name)))
  | None -> Tool.Result.error (not_run tool)
  | Some _ when host.active ->
    Tool.Result.error
      (sprintf
         "%s is the session's active host: call %s directly"
         host.name
         tool)
  | Some _ when String.equal tool "bash" && starts_job args ->
    Tool.Result.error
      (sprintf
         "background jobs run on the session's active host only: run it on %s \
          in the foreground (without background)"
         host.name)
  | Some inner ->
    (match cwd_on host cwd with
     | Error message -> Tool.Result.error message
     | Ok cwd ->
       let context =
         { context with
           host = Some host.id
         ; cwd
         ; home = (fun () -> host.home)
         ; background = None
         ; stdin = None
         }
       in
       (match context.approve context inner args with
        | Some refusal -> refusal
        | None -> Tool.execute_via context inner args))
;;

let run ~hosts (context : Tool.Context.t) call =
  let hosts = hosts () in
  match
    ( Tool_args.string_opt call "host"
    , Tool_args.string_opt call "tool"
    , Json.member "args" call )
  with
  | None, None, _ -> Tool.Result.ok (listing hosts)
  | None, Some _, _ ->
    Tool.Result.error ("on_host needs host with tool. " ^ listing hosts)
  | Some key, tool, inner_args ->
    (match find hosts key with
     | Error message -> Tool.Result.error message
     | Ok host ->
       (match tool, inner_args with
        | None, _ ->
          Tool.Result.error
            (sprintf
               "on_host needs tool and args with host, e.g. %s"
               (String.substr_replace_all
                  example
                  ~pattern:"NAME"
                  ~with_:host.name))
        | Some tool, None ->
          Tool.Result.error
            (sprintf
               "on_host needs args: %s's own arguments, as for a normal call"
               tool)
        | Some tool, Some (`Object _ as args) ->
          run_on context host ~tool ~args ~cwd:(Tool_args.string_opt call "cwd")
        | Some _, Some _ ->
          Tool.Result.error
            "on_host's args must be an object: the tool's arguments"))
;;

let create ~hosts () = { Tool.spec; run = run ~hosts }
