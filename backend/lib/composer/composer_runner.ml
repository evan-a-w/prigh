open! Core
open! Import

type live = Live : (module Adapter.S with type t = 'a) * 'a -> live

module Warming = struct
  type t =
    { host : string
    ; cwd : string
    ; done_ : unit Promise.t
    ; cancel : Cancellation.t
    }
end

type t =
  { sw : Switch.t
  ; adapter : Adapter.t
  ; spawn : Adapter.Start.t -> Adapter.Launch.t -> Line_transport.t Or_error.t
  ; start : cwd:string -> Adapter.Start.t
  ; interrupt_grace : Time_float.Span.t option
  ; request_timeout : Time_float.Span.t option
  ; host : unit -> string
  ; host_id : string option
  ; home : (unit -> string option) option
  ; depth : int
  ; agent_id : string option
  ; wake : unit -> Message.t list
  ; mutable live : live option
  ; mutable live_host : string option
  ; mutable live_cwd : string option
  ; mutable live_login : Account_ref.t option
    (** the login of prigh's the live process was given ([Start.login]) *)
  ; mutable login_refused : string option
    (** the token the last process was refused with ({!Adapter.Login_renewal}) *)
  ; mutable login_dropped : Account_ref.t option
    (** a login of prigh's refused even after a refresh: not handed again *)
  ; mutable thread_id : string option
  ; mutable resumed : string option
  ; mutable fresh : bool
  ; mutable handoff_from : Engine_choice.t option
  ; mutable last_place : (string * string) option
  ; mutable turns : int
  ; mutable thread_model : string option (* the model the CLI said it runs *)
  ; on_thread : string -> unit
  ; on_start : Adapter.Start.t -> unit Or_error.t -> unit
  ; on_prewarm : Adapter.Start.t -> Adapter.Catalog.t Or_error.t -> unit
  ; mutable on_title : (string -> unit) option
  ; checkpoint : (Json.t -> Tool.Result.t) option (* runs [Checkpoint.op] *)
  ; mutable not_git : (string * string) option
    (* a cwd whose files have no checkpoints, and why *)
  ; mutable checkpoint_noted : bool
  ; mutable transport_noted : bool (* [Line_transport.notice] was shown *)
  ; mutable no_sandbox :
      string option (* the live process's [Start.no_sandbox] *)
  ; mutable sandbox_noted : bool
  ; mutable forking : string option (* the thread a new one forks *)
  ; mutable services : Engine_intf.Services.t option (* the running turn's *)
  ; mutable cancel : Cancellation.t
  ; mutable run_cancel : Cancellation.t (* the run [cancel] is part of *)
  ; mutable models : Model.t list (* the harness's, as it last said *)
  ; mutable context : int option * int option
  ; mutable known : Capabilities.t option (* the last process's *)
  ; catalog : unit -> Adapter.Catalog.t option
  ; on_change : unit -> unit
  ; on_autonomous : (unit -> unit) option
  ; injected_errors : string Queue.t String.Table.t
    (* failed injected MCP tool texts in the current turn, by tool name, in
       case the harness drops the error text on [Tool_end] *)
  ; extra_dirs : unit -> string list
  ; mutable preapproved : (string * Json.t) list
    (** injected prigh MCP calls already approved in this turn's outer CLI
        permission check, consumed once by {!injected} to avoid prompting
        twice *)
  ; mutable warming : Warming.t option (* a [prewarm] starting *)
  ; mutable epoch : int (* [close]s: a start that outlived one stops *)
  }

let create
      ~sw
      ~adapter
      ~spawn
      ?spawn_start
      ?(start = fun ~cwd -> Adapter.Start.default ~cwd)
      ?interrupt_grace
      ?request_timeout
      ?(on_thread = ignore)
      ?(on_start = fun _ _ -> ())
      ?(on_prewarm = fun _ _ -> ())
      ?(host = fun () -> "local")
      ?host_id
      ?home
      ?(depth = 0)
      ?agent_id
      ?(wake = fun () -> [])
      ?handoff_from
      ?checkpoint
      ?(catalog = fun () -> None)
      ?(on_change = ignore)
      ?on_autonomous
      ?(extra_dirs = fun () -> [])
      ()
  =
  { sw
  ; adapter
  ; spawn =
      (match spawn_start with
       | Some f -> f
       | None -> fun _start launch -> spawn launch)
  ; start
  ; interrupt_grace
  ; request_timeout
  ; host
  ; host_id
  ; home
  ; depth
  ; agent_id
  ; wake
  ; live = None
  ; live_host = None
  ; live_cwd = None
  ; live_login = None
  ; login_refused = None
  ; login_dropped = None
  ; thread_id = None
  ; resumed = None
  ; fresh = false
  ; handoff_from
  ; last_place = None
  ; turns = 0
  ; thread_model = None
  ; on_thread
  ; on_start
  ; on_prewarm
  ; on_title = None
  ; checkpoint
  ; not_git = None
  ; checkpoint_noted = false
  ; transport_noted = false
  ; no_sandbox = None
  ; sandbox_noted = false
  ; forking = None
  ; services = None
  ; cancel = Cancellation.never
  ; run_cancel = Cancellation.never
  ; models = []
  ; context = None, None
  ; known = None
  ; catalog
  ; on_change
  ; on_autonomous
  ; injected_errors = String.Table.create ()
  ; extra_dirs
  ; preapproved = []
  ; warming = None
  ; epoch = 0
  }
;;

let thread_id t = t.thread_id

let has_open_work t =
  match t.live with
  | Some (Live ((module L), a)) -> L.alive a && L.has_open_work a
  | None -> false
;;

let alive t =
  match t.live with
  | Some (Live ((module L), a)) -> L.alive a
  | None -> false
;;

let await_exit t =
  match t.live with
  | Some (Live ((module L), a)) -> L.await_exit a
  | None -> ()
;;

let close t =
  t.epoch <- t.epoch + 1;
  Option.iter t.warming ~f:(fun w -> Cancellation.cancel w.cancel);
  t.warming <- None;
  Option.iter t.live ~f:(fun (Live ((module A), a)) ->
    t.known <- Some (A.current_capabilities a);
    A.stop a);
  t.live <- None;
  t.live_cwd <- None;
  t.models <- []
;;

let context
      t
      (s : Engine_intf.Services.t)
      ~cancel
      ?(cwd = s.cwd)
      ~call_id
      ?agent_id
      ()
  =
  let agent_id =
    match t.agent_id, agent_id with
    | None, p -> p
    | Some a, None -> Some a
    | Some a, Some p -> Some (a ^ "/" ^ p)
  in
  Tool.Context.create
    ~cancel
    ~execute:s.execute
    ~call_id
    ~depth:t.depth
    ?agent_id
    ?host:t.host_id
    ?home:t.home
    ~tools:s.tools
    ~emit:s.emit
    ~background:s.background
    ~halt:s.halt
    ~approve:s.approve
    ~ask_user:s.ask_user
    ~ui:s.ui
    ~env:s.env
    ~cwd
    ()
;;

let mcp_calls = ref 0

let take_preapproved t ~tool ~args =
  let key = Json.to_string args in
  let rec remove acc = function
    | [] -> false
    | (name, a) :: rest
      when String.equal name tool && String.equal (Json.to_string a) key ->
      t.preapproved <- List.rev_append acc rest;
      true
    | item :: rest -> remove (item :: acc) rest
  in
  remove [] t.preapproved
;;

(* prigh's tools called over MCP: approved and run as the harness engine's
   calls are. *)
let injected t (s : Engine_intf.Services.t) =
  let (module A : Adapter.S) = t.adapter in
  let native_questions = A.native_questions in
  Injected_tools.create
    ~tools:(Injected_tools.select ~native_questions s.tools)
    ~execute:(fun tool args ->
      match t.services with
      | None -> Tool_result.error "no turn is running"
      | Some s ->
        incr mcp_calls;
        let run_cancel = t.run_cancel in
        let ctx =
          context t s ~cancel:t.cancel ~call_id:(sprintf "mcp-%d" !mcp_calls) ()
        in
        let refusal =
          if take_preapproved t ~tool:(Tool.name tool) ~args
          then None
          else s.approve ctx tool args
        in
        (match refusal with
         | Some refused -> refused
         | None
           when Cancellation.is_cancelled ctx.cancel
                || Cancellation.is_cancelled run_cancel ->
           (* Approved after its turn ended: nobody waits for it now. *)
           Tool_result.error "[cancelled]: its turn ended"
         | None ->
           let result = s.execute ctx tool args in
           if result.is_error && not (String.is_empty result.text)
           then (
             let q =
               Hashtbl.find_or_add
                 t.injected_errors
                 (Tool.name tool)
                 ~default:Queue.create
             in
             Queue.enqueue q result.text);
           result))
;;

let settings t (input : Engine_intf.Turn.Input.t) : Adapter.Settings.t =
  { model = input.model
  ; thinking = input.thinking
  ; approval = input.approval
  ; plan = input.plan
  ; extra_dirs = t.extra_dirs ()
  }
;;

(* The thread [a] is on, once known: recorded when it is not the one it was
   asked to continue (that one is recorded already). A thread other than
   that one is new, so it is handed the whole session. *)
let note_thread
      t
      (s : Engine_intf.Services.t)
      (type a)
      (module A : Adapter.S with type t = a)
      (a : a)
  =
  match A.thread_id a with
  | Some id when not (Option.equal String.equal (Some id) t.thread_id) ->
    t.thread_id <- Some id;
    if not (Option.equal String.equal (Some id) t.resumed)
    then (
      (match t.forking with
       | Some parent ->
         (* A fork of [parent] at the session's last run: it has seen what
            that run had. *)
         t.forking <- None;
         Option.iter
           (List.last
              (List.filter (Session.external_turns s.session) ~f:(fun turn ->
                 String.equal turn.thread_id parent)))
           ~f:(fun turn ->
             ignore
               (Session.record_external_turn
                  s.session
                  { turn with thread_id = id }
                : Session.Entry.t))
       | None -> if Option.is_some t.resumed then t.fresh <- true);
      t.on_thread id)
  | Some _ | None -> ()
;;

let start_process t (s : Engine_intf.Services.t) (start : Adapter.Start.t) =
  let (module A) = t.adapter in
  let launch = Turn_timings.phase "launch" (fun () -> A.launch start) in
  match Turn_timings.phase "spawn" (fun () -> t.spawn start launch) with
  | Error e -> Error (`Spawn e)
  | Ok transport ->
    Option.iter (Line_transport.notice transport) ~f:(fun notice ->
      if not t.transport_noted
      then (
        t.transport_noted <- true;
        s.notice notice));
    let injected = injected t s in
    (* A harness with no MCP in its protocol reaches prigh's tools through
       the bridge on its host. *)
    let start =
      match Line_transport.mcp transport with
      | None -> start
      | Some mcp ->
        mcp.serve injected;
        { start with
          mcp_servers =
            start.mcp_servers
            @ [ ( Injected_tools.server_name
                , Injected_tools.Bridge.server_json mcp.bridge )
              ]
        }
    in
    (match
       Turn_timings.phase "handshake" (fun () ->
         A.start
           ~sw:t.sw
           ?interrupt_grace:t.interrupt_grace
           ?request_timeout:t.request_timeout
           ~injected
           transport
           start)
     with
     | exception exn ->
       (* Cancelled while it started (Esc): it would run for nobody. *)
       Line_transport.stop transport;
       raise exn
     | Ok a ->
       t.on_start start (Ok ());
       t.no_sandbox <- start.no_sandbox;
       t.resumed <- start.resume;
       t.forking <- Option.some_if start.fork start.resume |> Option.join;
       (* A new thread has seen nothing: whatever an older one saw. *)
       if Option.is_none start.resume then t.fresh <- true;
       Ok (Live ((module A), a))
     | Error e ->
       Line_transport.stop transport;
       t.on_start start (Error e);
       Error (`Start e))
;;

(* Whether the session was compacted (prigh summarised it: a [Compaction]
   entry) since [thread]'s last run: the thread holds the context the summary
   replaced, so a new one is started, handed the compacted session. *)
let compacted_since session ~thread =
  match
    List.last
      (List.filter (Session.external_turns session) ~f:(fun turn ->
         String.equal turn.thread_id thread))
  with
  | None -> false
  | Some last ->
    List.drop_while (Session.active_path session) ~f:(fun e ->
      not (String.equal e.id last.through))
    |> List.exists ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Compaction _ -> true
      | _ -> false)
;;

(* How to continue [resume] after the session was rewound past some of its
   runs on it (they are off the active path): as it is ([`Whole]), rolled
   back to the end of its last run still on the path, or not at all (a new
   thread, handed the session) when it cannot be rolled back there. Runs off
   the path with a later run on it were rolled back before that one ran. *)
let rollback_plan t session ~resume =
  let (module A : Adapter.S) = t.adapter in
  match resume with
  | None -> `Whole
  | Some thread ->
    let of_thread turns =
      List.filter turns ~f:(fun (turn : Session.External_turn.t) ->
        String.equal turn.thread_id thread)
    in
    let on_path = of_thread (Session.external_turns session) in
    let on turn = List.mem on_path turn ~equal:Session.External_turn.equal in
    let rec has = function
      | [] -> []
      | turn :: rest ->
        if (not (on turn)) && List.exists rest ~f:on
        then has rest
        else turn :: has rest
    in
    let has = has (of_thread (Session.external_turns ~all:true session)) in
    let kept = List.length on_path in
    if
      not
        (List.is_prefix has ~prefix:on_path ~equal:Session.External_turn.equal)
    then `Nothing
    else if List.length has = kept
    then `Whole
    else (
      match A.rollback with
      | Unsupported -> `Nothing
      | Revert ->
        let (first : Session.External_turn.t) = List.nth_exn has kept in
        `Revert (Option.value first.first_turn_id ~default:first.turn_id)
      | Resume_at ->
        (match List.last on_path with
         | Some ({ mark = Some _; _ } as last) ->
           `At
             { Session.Fork_point.turn_id = Some last.turn_id
             ; mark = last.mark
             }
         | Some { mark = None; _ } | None -> `Nothing))
;;

(* The running cost total [thread] reported after its last recorded turn. *)
let cost_total_of session thread =
  List.rev (Session.external_turns session)
  |> List.find_map ~f:(fun (turn : Session.External_turn.t) ->
    if String.equal turn.thread_id thread then turn.cost_total else None)
;;

(* A harness that takes prigh's login only when it starts gets a new process
   (on the same thread) when its token is about to expire or was refused;
   refused again after a refresh, the login is not handed again. *)
let renew_login t (s : Engine_intf.Services.t) ~login =
  match t.live with
  | Some (Live ((module A), a)) when A.alive a ->
    (match A.renewal a with
     | None -> t.login_refused <- None
     | Some renewal ->
       A.stop a;
       t.live <- None;
       (match renewal, login with
        | Expiring, _ -> t.login_refused <- None
        | Refused _, Some login when Option.is_some t.login_refused ->
          t.login_refused <- None;
          t.login_dropped <- Some (Cli_credential.account login);
          let provider = Cli_credential.provider login in
          s.notice
            (sprintf
               "%s was refused prigh's %s login %S again, after a refresh: \
                this session runs on %s's own login on its tool host from now \
                on (/login %s logs in again, for new sessions)."
               A.display_name
               (Provider_id.display_name provider)
               (Cli_credential.label login)
               A.display_name
               (Provider_id.to_string provider))
        | Refused token, _ -> t.login_refused <- Some token))
  | Some _ | None -> ()
;;

let live_now
      ?(on_result = fun _ _ -> ())
      t
      (s : Engine_intf.Services.t)
      (input : Engine_intf.Turn.Input.t)
      ~starting
  =
  t.on_title <- Some s.set_title;
  match input.kind, t.live with
  | Autonomous, Some (Live ((module A), a) as live) when A.alive a -> Ok live
  | _ ->
    let host = t.host () in
    (* The session moved to another tool host: its harness runs there now,
     on a thread of its own there. *)
    (match t.live with
     | Some (Live ((module A), a))
       when not (Option.equal String.equal t.live_host (Some host)) ->
       A.stop a;
       t.live <- None;
       t.live_cwd <- None;
       t.thread_id <- None;
       t.models <- []
     | Some (Live ((module A), a))
       when A.starts_early
            && not (Option.equal String.equal t.live_cwd (Some s.cwd)) ->
       A.stop a;
       t.live <- None;
       t.live_cwd <- None
     | Some _ | None -> ());
    let base =
      Turn_timings.phase "login" (fun () ->
        let base = t.start ~cwd:s.cwd in
        renew_login t s ~login:base.login;
        base)
    in
    let base =
      match base.login, t.login_dropped with
      | Some login, Some dropped
        when Account_ref.equal (Cli_credential.account login) dropped ->
        { base with login = None }
      | _ -> base
    in
    let login = Option.map base.login ~f:Cli_credential.account in
    (* The user chose another login for the harness: a process on it, on the
     same thread. *)
    (match t.live with
     | Some (Live ((module A), a))
       when not (Option.equal Account_ref.equal t.live_login login) ->
       A.stop a;
       t.live <- None;
       t.live_cwd <- None;
       t.models <- []
     | Some _ | None -> ());
    (match t.live with
     | Some (Live ((module A), a)) when A.alive a -> Ok (Live ((module A), a))
     | _ ->
       starting ();
       Turn_timings.phase "process"
       @@ fun () ->
       let epoch = t.epoch in
       let (module A) = t.adapter in
       let resume =
         match Option.first_some t.thread_id base.resume with
         | Some thread when compacted_since s.session ~thread ->
           t.thread_id <- None;
           t.fresh <- true;
           None
         | resume -> resume
       in
       let fork = base.fork && Option.is_none t.thread_id in
       let resume, at, revert =
         if fork
         then resume, base.at, None
         else (
           match rollback_plan t s.session ~resume with
           | `Whole -> resume, None, None
           | `At at -> resume, Some at, None
           | `Revert before_turn_id -> resume, None, Some before_turn_id
           | `Nothing ->
             t.fresh <- true;
             None, None, None)
       in
       let start =
         { base with
           cost_total =
             Option.first_some
               (Option.bind resume ~f:(cost_total_of s.session))
               base.cost_total
         ; model = input.model
         ; thinking = input.thinking
         ; approval = input.approval
         ; plan = input.plan
         ; resume
         ; fork = fork && Option.is_some resume
         ; at
         ; login_refused = t.login_refused
         ; extra_dirs =
             List.dedup_and_sort
               (base.extra_dirs @ t.extra_dirs ())
               ~compare:String.compare
         }
       in
       let started =
         match start_process t s start, resume with
         | Error (`Start e), Some thread ->
           (* Its thread cannot be continued (gone, or in use elsewhere): a new
           one, handed the whole session. *)
           s.notice
             (sprintf
                "%s could not continue its thread %s (%s): starting a new one, \
                 with the session handed over"
                A.display_name
                thread
                (Error.to_string_hum e));
           t.thread_id <- None;
           t.fresh <- true;
           start_process
             t
             s
             { start with
               resume = None
             ; fork = false
             ; at = None
             ; cost_total = None
             }
         | Ok (Live ((module L), a)), Some thread when Option.is_some revert ->
           (* The session was rewound: its thread drops the runs it lost. *)
           (match L.revert a ~before_turn_id:(Option.value_exn revert) with
            | Ok () -> Ok (Live ((module L), a))
            | Error e ->
              s.notice
                (sprintf
                   "%s could not roll its thread %s back to the rewound \
                    session (%s): starting a new one, with the session handed \
                    over"
                   A.display_name
                   thread
                   (Error.to_string_hum e));
              L.stop a;
              t.thread_id <- None;
              t.fresh <- true;
              start_process
                t
                s
                { start with
                  resume = None
                ; fork = false
                ; at = None
                ; cost_total = None
                })
         | started, _ -> started
       in
       (match started with
        | Ok (Live ((module L), a)) when t.epoch <> epoch ->
          (* Closed while it started (the session was left): nobody runs it. *)
          L.stop a;
          Or_error.errorf "%s was stopped while it started" A.display_name
        | Ok (Live ((module L), a) as live) ->
          (match L.models a with
           | [] -> ()
           | models -> t.models <- models);
          L.on_change a (fun () ->
            (match L.models a with
             | [] -> ()
             | models -> t.models <- models);
            t.on_change ());
          Option.iter t.on_autonomous ~f:(L.on_autonomous a);
          L.on_title a (fun title ->
            Option.iter t.on_title ~f:(fun f -> f title));
          t.live <- Some live;
          t.live_host <- Some host;
          t.live_cwd <- Some s.cwd;
          t.live_login <- login;
          on_result
            start
            (Ok { Adapter.Catalog.models = L.models a; limits = L.limits a });
          t.on_change ();
          Ok live
        | Error (`Spawn e | `Start e) ->
          if t.epoch = epoch then on_result start (Error e);
          Error e))
;;

(* A process [prewarm] is starting for the session is this turn's. *)
let live t s input ~starting =
  Option.iter t.warming ~f:(fun (w : Warming.t) ->
    Turn_timings.phase "warming_up" (fun () -> Promise.await w.done_));
  live_now t s input ~starting
;;

let prewarm t (s : Engine_intf.Services.t) input =
  let host = t.host () in
  let (module A) = t.adapter in
  if A.starts_early && Option.is_none t.services
  then (
    let up =
      match t.live with
      | Some (Live ((module L), a)) ->
        L.alive a
        && Option.equal String.equal t.live_host (Some host)
        && Option.equal String.equal t.live_cwd (Some s.cwd)
      | None -> false
    in
    if not up
    then (
      let already_warming =
        match t.warming with
        | Some w when String.equal w.host host && String.equal w.cwd s.cwd ->
          true
        | Some w ->
          t.epoch <- t.epoch + 1;
          Cancellation.cancel w.cancel;
          t.warming <- None;
          false
        | None -> false
      in
      if not already_warming
      then (
        Option.iter t.live ~f:(fun (Live ((module L), a)) ->
          L.stop a;
          t.live <- None;
          t.live_cwd <- None;
          if not (Option.equal String.equal t.live_host (Some host))
          then (
            t.thread_id <- None;
            t.models <- []));
        let done_, warmed = Promise.create () in
        let cancel = Cancellation.create () in
        let w = { Warming.host; cwd = s.cwd; done_; cancel } in
        t.warming <- Some w;
        Guarded.fork
          ~sw:t.sw
          ~what:"starting an external harness early"
          (fun () ->
             Exn.protect
               ~finally:(fun () ->
                 (match t.warming with
                  | Some cur when phys_equal cur w -> t.warming <- None
                  | Some _ | None -> ());
                 Promise.resolve warmed ())
               ~f:(fun () ->
                 (* What goes wrong is said by the turn, which tries again. *)
                 ignore
                   (Cancellation.protect cancel ~f:(fun () ->
                      live_now
                        ~on_result:(fun start result ->
                          if not (Cancellation.is_cancelled cancel)
                          then t.on_prewarm start result)
                        t
                        s
                        input
                        ~starting:ignore)
                    : live Or_error.t option))))))
;;

(* What the harness has not seen of the session, handed to it before the
   turn's prompt: everything when it starts a thread on a session with
   history (after another adapter, on another host, or when its thread
   could not be continued), else what came after its last turn. Recorded in
   the session. *)
let handoff t (s : Engine_intf.Services.t) ~pending =
  let (module A) = t.adapter in
  match Adapter_id.of_string A.id with
  | Error _ -> None
  | Ok to_ when not (Handoff.needed to_) -> None
  | Ok to_ ->
    let from =
      match t.handoff_from with
      | Some _ as from -> from
      | None ->
        (match Session.threads s.session with
         | thread :: _ when not (String.equal thread.adapter A.id) ->
           Result.ok (Adapter_id.of_string thread.adapter)
           |> Option.map ~f:(fun adapter ->
             { Engine_choice.engine = Composer; adapter = Some adapter })
         | _ :: _ | [] -> None)
    in
    let from_host, from_cwd =
      match t.last_place with
      | Some (host, cwd) -> Some host, Some cwd
      | None -> None, None
    in
    let host = t.host () in
    let handoff =
      Handoff.build
        ~session:s.session
        ~from
        ~fresh:t.fresh
        ~pending
        ?from_host
        ?from_cwd
        ~to_
        ~host
        ~cwd:s.cwd
        ()
    in
    t.fresh <- false;
    t.handoff_from <- None;
    t.last_place <- Some (host, s.cwd);
    Option.iter handoff ~f:(fun (handoff : Handoff.t) ->
      ignore
        (Session.record_handoff
           s.session
           ~from_adapter:
             (Option.map from ~f:(fun (from : Engine_choice.t) ->
                match from.adapter with
                | Some adapter -> Adapter_id.to_string adapter
                | None -> Engine_id.to_string from.engine))
           ~to_adapter:A.id
           ~text:handoff.text
         : Session.Entry.t));
    handoff
;;

let input_of (messages : Message.t list) : Adapter.Input.t =
  let users =
    List.filter_map messages ~f:(function
      | User u -> Some u
      | Assistant _ | Tool_result _ -> None)
  in
  { text = String.concat ~sep:"\n\n" (List.map users ~f:(fun u -> u.text))
  ; images = List.concat_map users ~f:(fun u -> u.images)
  }
;;

(* A call the approvals know nothing of: asked about in every mode but
   [auto]. *)
let stand_in name : Tool.t =
  { spec =
      { name
      ; description = ""
      ; parameters = `Object []
      ; parallel_safe = false
      ; destructive = Always
      ; on_host = true
      ; freeform = None
      ; native = None
      }
  ; run = (fun _ _ -> Tool_result.error "run by the external harness")
  }
;;

let reply_of_refusal (r : Tool_result.t) : Adapter_event.Approval_reply.t =
  match r.text with
  | "[cancelled]" -> Cancel
  | "[denied by user]" -> Deny None
  | text ->
    (match
       String.chop_prefix text ~prefix:"[denied by user] Their reason: "
     with
     | Some reason -> Deny (Some reason)
     | None -> Deny (Some text))
;;

let kind_name : Adapter_event.Tool_kind.t -> string = function
  | Command -> "command"
  | File_change -> "file_change"
  | Read -> "read"
  | Search -> "search"
  | Web_search -> "web_search"
  | Mcp { server; tool } -> Mcp_tools.tool_name ~server ~tool
  | Todo -> "todo"
  | Subagent -> "subagent"
  | Other name -> name
;;

(* The harness's plan, put to the user: plan mode's refusals would deny
   leaving it otherwise. *)
let approve_plan (s : Engine_intf.Services.t) ~display_name ~request_id ~summary
  : Adapter_event.Approval_reply.t
  =
  let yes = "Start working" in
  match
    s.ask_user
      ~call_id:request_id
      ~timeout_seconds:None
      [ { question = sprintf "%s's plan:\n\n%s" display_name summary
        ; header = Some "Plan"
        ; choices =
            [ { label = yes; description = None }
            ; { label = "Keep planning"; description = None }
            ]
        ; multiple = false
        }
      ]
  with
  | Answered [ [ answer ] ] when String.equal answer yes -> Allow
  | Answered [ [ "Keep planning" ] ] -> Deny None
  | Answered answers ->
    Deny (Some (String.concat ~sep:"\n" (List.concat answers)))
  | Declined -> Deny None
  | Cancelled | Timed_out _ | Unavailable _ -> Cancel
;;

let checkpoints_made = ref 0

(* The files now, kept under a ref for a rewind to return to; [None]
   without checkpoints (none asked for, not in a git repository, an older
   prigh on the host). *)
let capture t (s : Engine_intf.Services.t) ~which =
  match t.checkpoint, t.not_git with
  | None, _ -> None
  | Some _, Some (cwd, _) when String.equal cwd s.cwd -> None
  | Some exec, _ ->
    incr checkpoints_made;
    let ref =
      sprintf
        "%s%s-%d-%s"
        (Checkpoint.session_prefix ~session:(Session.id s.session))
        (Int63.to_string (Time_ns.to_int63_ns_since_epoch (Time_ns.now ())))
        !checkpoints_made
        which
    in
    (match Checkpoint.Client.capture exec ~ref:(Some ref) with
     | Ok (Snapshot { commit; _ }) ->
       t.not_git <- None;
       commit
     | Ok (Not_git why) ->
       t.not_git <- Some (s.cwd, why);
       None
     | Error e ->
       if not t.checkpoint_noted
       then (
         t.checkpoint_noted <- true;
         s.notice
           (sprintf
              "prigh could not checkpoint the files, so this session cannot \
               rewind them: %s"
              (Error.to_string_hum e)));
       None)
;;

(* Said once, in the first turn its sandbox would have mattered in. *)
let note_sandbox
      t
      (s : Engine_intf.Services.t)
      (input : Engine_intf.Turn.Input.t)
  =
  match t.no_sandbox, input.approval with
  | None, _ | Some _, Auto -> ()
  | Some where, (Ask | Auto_edit | Smart) ->
    if not t.sandbox_noted
    then (
      t.sandbox_noted <- true;
      let (module A) = t.adapter in
      s.notice
        (sprintf
           "%s's sandbox cannot run on %s: it runs without it, and prigh still \
            asks before its commands and edits as the session's mode says"
           A.display_name
           where))
;;

let rec run_turn
          t
          (s : Engine_intf.Services.t)
          ~cancel
          (input : Engine_intf.Turn.Input.t)
  =
  let (module A) = t.adapter in
  (* Cancelled when the run is, and when the turn ends however it does:
     what prigh's tools were asked over MCP for it is withdrawn then. *)
  let turn_cancel = Cancellation.create () in
  Fiber.fork_daemon ~sw:t.sw (fun () ->
    Fiber.first
      (fun () -> Cancellation.await cancel)
      (fun () -> Cancellation.await turn_cancel);
    Cancellation.cancel turn_cancel;
    `Stop_daemon);
  t.services <- Some s;
  t.cancel <- turn_cancel;
  t.run_cancel <- cancel;
  Hashtbl.clear t.injected_errors;
  t.preapproved <- [];
  (match Turn_timings.current () with
   | Some _ -> fun f -> f ()
   | None -> Turn_timings.with_ (Turn_timings.create ()))
  @@ fun () ->
  Exn.protect
    ~finally:(fun () ->
      Hashtbl.clear t.injected_errors;
      t.preapproved <- [];
      Cancellation.cancel turn_cancel)
    ~f:(fun () ->
      match run_turn' t s ~cancel input with
      | added -> added
      | exception exn ->
        (* Its events may still come, for a turn that is no longer
           anyone's: the next turn starts a new process instead. *)
        t.services <- None;
        close t;
        raise exn)

and run_turn'
      t
      (s : Engine_intf.Services.t)
      ~cancel
      (input : Engine_intf.Turn.Input.t)
  =
  let (module A) = t.adapter in
  let sent = ref false in
  let last_turn = ref None in
  let first_turn = ref None in
  let mark = ref None in
  let before = ref None in
  let cost_total = ref None in
  let recorder =
    Composer_recorder.create
      ~adapter:A.id
      ~model:(Option.first_some input.model t.thread_model)
      ~cwd:s.cwd
      ~emit:s.emit
      ~notice:s.notice
      ~prompts:input.prompts
  in
  let fail message =
    Composer_recorder.feed
      recorder
      (Turn_end { turn_id = None; stop = Failed { kind = Other; message } })
  in
  let timings = Turn_timings.current () in
  (* Once a run: where the time went before the harness answered, in the
     trace. *)
  let reported = ref false in
  let report () =
    Option.iter timings ~f:(fun timings ->
      if not !reported
      then (
        reported := true;
        Trace.add_attrs
          ~under:"prompt"
          [ "first_output_s", Float (Turn_timings.elapsed timings)
          ; ( "start_phases"
            , String (Turn_timings.to_string_hum (Turn_timings.phases timings))
            )
          ]))
  in
  (* The files are checkpointed while the harness starts: it is sent
     nothing before they are. Skipped for adapters that cannot roll back. *)
  let before_checkpoint =
    match input.kind, A.rollback, t.checkpoint, t.not_git with
    | (Compact _ | Autonomous), _, _, _
    | Prompt, Unsupported, _, _
    | Prompt, _, None, _ -> None
    | Prompt, _, Some _, Some (cwd, _) when String.equal cwd s.cwd -> None
    | Prompt, _, Some _, _ ->
      Some
        (Fiber.fork_promise ~sw:t.sw (fun () ->
           Turn_timings.phase ~alongside:true "checkpoint" (fun () ->
             capture t s ~which:"before")))
  in
  (match
     Cancellation.protect cancel ~f:(fun () ->
       live t s input ~starting:(fun () -> Composer_recorder.starting recorder))
   with
   | None ->
     Composer_recorder.feed
       recorder
       (Turn_end { turn_id = None; stop = Interrupted })
   | Some (Error e) ->
     let why = Error.to_string_hum e in
     fail
       (if String.is_prefix why ~prefix:A.display_name
        then why
        else sprintf "%s could not start: %s" A.display_name why)
   | Some (Ok (Live ((module L), a))) ->
     note_thread t s (module L) a;
     note_sandbox t s input;
     if Option.is_none input.model
     then
       Option.iter
         (List.hd (L.models a))
         ~f:(fun (m : Model.t) -> Composer_recorder.default_model recorder m.id);
     let settings = settings t input in
     let leftover = Queue.create () in
     let handoff =
       ref
         (Option.map
            (Turn_timings.phase "handoff" (fun () ->
               handoff t s ~pending:(List.length input.prompts)))
            ~f:(fun (h : Handoff.t) -> h.text))
     in
     Option.iter before_checkpoint ~f:(fun p ->
       before
       := Turn_timings.phase "checkpoint_wait" (fun () -> Promise.await_exn p));
     let with_handoff (input : Adapter.Input.t) =
       match !handoff with
       | None -> input
       | Some text ->
         handoff := None;
         { input with text = text ^ "\n\nThe user's message:\n\n" ^ input.text }
     in
     let rec turn ~first messages =
       Composer_recorder.waiting recorder;
       let result =
         match input.kind with
         | Autonomous when first -> Ok ()
         | Compact { instructions } when first ->
           Turn_timings.phase "send" (fun () ->
             L.compact a settings ~instructions)
         | Compact _ | Prompt | Autonomous ->
           Turn_timings.phase "send" (fun () ->
             L.send a settings (with_handoff (input_of messages)))
       in
       match result with
       | Error e -> fail (Error.to_string_hum e)
       | Ok () ->
         let sent_at = Time_float.now () in
         let output () =
           if not !reported
           then (
             Option.iter timings ~f:(fun timings ->
               Turn_timings.record
                 timings
                 "first_output"
                 ~seconds:
                   (Time_float.Span.to_sec
                      (Time_float.diff (Time_float.now ()) sent_at)));
             report ())
         in
         sent := true;
         cost_total := Some (fun () -> L.cost_total a);
         let requests = String.Table.create () in
         let stop = ref None in
         (* Each request waits in a fiber of its own, until answered,
            withdrawn, the run cancelled or the turn ended: its dialog
            closes then ({!Agent.confirm} and [ask_user] clean up when
            cancelled). Not daemons: the turn waits for them to close. *)
         let handle_request ~sw request_id f =
           let c = Cancellation.create () in
           Hashtbl.set requests ~key:request_id ~data:c;
           Fiber.fork ~sw (fun () ->
             Fiber.first
               (fun () -> f c)
               (fun () ->
                  Fiber.first
                    (fun () -> Cancellation.await c)
                    (fun () -> Cancellation.await cancel));
             Hashtbl.remove requests request_id)
         in
         let rec events ~sw =
           let event = L.next_event a in
           (match event with
            | Notice _
            | Usage _
            | Turn_started _
            | Thread_started _
            | Retrying _
            | Request_withdrawn _
            | Session_title _ -> ()
            | _ -> output ());
           match event with
           | Turn_end { stop = st; turn_id } as e ->
             Composer_recorder.feed recorder e;
             Option.iter turn_id ~f:(fun id ->
               if Option.is_none !first_turn then first_turn := Some id;
               last_turn := Some id);
             stop := Some st
           | Approval_request { request_id; tool_id; kind; summary; input; _ }
             ->
             handle_request ~sw request_id (fun c ->
               let reply =
                 match kind with
                 | Other "ExitPlanMode" ->
                   approve_plan
                     s
                     ~display_name:A.display_name
                     ~request_id
                     ~summary
                 | _ ->
                   (* What is judged is what the harness asks to run: the
                      request's own input, not the call it announced
                      (a hook or the harness may have changed it). The
                      announced call only says what it is called and which
                      subagent runs it. *)
                   let native_name, parent =
                     match
                       Option.bind tool_id ~f:(fun tool_id ->
                         Composer_recorder.call_started recorder ~tool_id)
                     with
                     | Some ((announced : Canonical_tool.t), parent) ->
                       announced.native_name, parent
                     | None -> kind_name kind, None
                   in
                   let canonical =
                     Canonical_tool.of_start ~kind ~native_name ~input
                   in
                   (* Where the harness runs it (Codex says), for the
                      rules: else the session's. *)
                   let cwd =
                     match Json.member "cwd" input with
                     | Some (`String cwd) when Filename.is_absolute cwd ->
                       Some cwd
                     | _ -> None
                   in
                   let tool =
                     Option.first_some
                       (List.find s.tools ~f:(fun t ->
                          String.equal (Tool.name t) canonical.name))
                       (Tools.find canonical.name)
                     |> Option.value_or_thunk ~default:(fun () ->
                       stand_in canonical.name)
                   in
                   let is_injected =
                     match kind with
                     | Mcp { server; tool = mcp_tool } ->
                       Option.is_some
                         (Injected_tools.canonical_of_mcp
                            ~server
                            ~tool:mcp_tool)
                     | _ -> false
                   in
                   let ctx =
                     context
                       t
                       s
                       ~cancel:c
                       ?cwd
                       ~call_id:(Option.value tool_id ~default:request_id)
                       ?agent_id:parent
                       ()
                   in
                   (match s.approve ctx tool canonical.arguments with
                    | None ->
                      if is_injected && not (Cancellation.is_cancelled c)
                      then
                        t.preapproved
                        <- (Tool.name tool, canonical.arguments)
                           :: t.preapproved;
                      Allow
                    | Some refused -> reply_of_refusal refused)
               in
               if not (Cancellation.is_cancelled c)
               then (
                 Option.iter tool_id ~f:(fun tool_id ->
                   Composer_recorder.note_approval recorder ~tool_id reply);
                 L.reply_approval a ~request_id reply));
             events ~sw
           | User_question { request_id; questions } ->
             handle_request ~sw request_id (fun c ->
               let reply =
                 s.ask_user ~call_id:request_id ~timeout_seconds:None questions
               in
               if not (Cancellation.is_cancelled c)
               then L.answer a ~request_id reply);
             events ~sw
           | Request_withdrawn { request_id } ->
             Option.iter
               (Hashtbl.find requests request_id)
               ~f:Cancellation.cancel;
             events ~sw
           | Session_title title ->
             s.set_title title;
             events ~sw
           | e ->
             let e =
               match e with
               | Tool_end ({ id; ok = false; output; _ } as te)
                 when not (Hashtbl.is_empty t.injected_errors) ->
                 let matched =
                   Option.bind
                     (Composer_recorder.call_canonical recorder ~tool_id:id)
                     ~f:(fun canonical ->
                       let matches_tool name =
                         String.equal canonical.name name
                         || String.is_substring
                              canonical.native_name
                              ~substring:
                                (sprintf
                                   "mcp_%s_%s"
                                   Injected_tools.server_name
                                   name)
                         || String.is_substring
                              canonical.native_name
                              ~substring:
                                (Injected_tools.server_name ^ "_" ^ name)
                         || String.equal canonical.native_name name
                       in
                       Hashtbl.keys t.injected_errors
                       |> List.find ~f:matches_tool
                       |> Option.bind ~f:(fun name ->
                         Option.bind
                           (Hashtbl.find t.injected_errors name)
                           ~f:(fun q ->
                             let text = Queue.dequeue q in
                             if Queue.is_empty q
                             then Hashtbl.remove t.injected_errors name;
                             text)))
                 in
                 (match matched with
                  | Some err_text when String.is_empty output ->
                    Adapter_event.Tool_end { te with output = err_text }
                  | Some _ | None -> e)
               | _ -> e
             in
             (match e with
              | Thread_started { model = Some model; _ } ->
                t.thread_model <- Some model
              | Usage { usage; cost_usd; _ } ->
                Option.iter (L.login a) ~f:(fun login ->
                  Cli_credential.record
                    login
                    usage
                    ~cost_usd:(Option.value cost_usd ~default:0.))
              | _ -> ());
             Composer_recorder.feed recorder e;
             events ~sw
         in
         let interrupt_on_cancel () =
           Cancellation.await cancel;
           L.interrupt a;
           Fiber.await_cancel ()
         in
         let steer () =
           let rec loop () =
             match s.steer () with
             | [] ->
               s.await_steer ();
               loop ()
             | messages ->
               (match L.steer a (input_of messages) with
                | Ok () ->
                  List.iter
                    messages
                    ~f:(Composer_recorder.user_message recorder)
                | Error _ -> Queue.enqueue_all leftover messages);
               loop ()
           in
           loop ()
         in
         Switch.run (fun sw ->
           Fiber.any [ (fun () -> events ~sw); interrupt_on_cancel; steer ];
           Hashtbl.iter requests ~f:Cancellation.cancel);
         note_thread t s (module L) a;
         mark := L.mark a;
         (match !stop with
          | Some Done when not (Cancellation.is_cancelled cancel) ->
            let next = Queue.to_list leftover @ s.steer () in
            Queue.clear leftover;
            let next =
              if List.is_empty next && Option.is_none (s.halt ())
              then t.wake ()
              else next
            in
            if (not (List.is_empty next)) && Option.is_none (s.halt ())
            then (
              List.iter next ~f:(Composer_recorder.user_message recorder);
              turn ~first:false next)
          | _ ->
            (* Not sent: kept in the session, for the user to send again. *)
            Queue.iter leftover ~f:(Composer_recorder.user_message recorder);
            Queue.clear leftover)
     in
     turn ~first:true input.prompts);
  (match t.live with
   | Some (Live ((module L), a)) ->
     (match L.models a with
      | [] -> ()
      | models -> t.models <- models)
   | None -> ());
  (* How full its context is: as it says; else the session's estimate, or
     the input its turn reported when less (Antigravity's sums the turn's
     requests). After a compaction, only as it says. *)
  (let tokens, window = Composer_recorder.context recorder in
   let old_window = snd t.context in
   let tokens =
     match input.kind with
     | Compact _ ->
       (match tokens with
        | Some _ -> tokens
        | None ->
          (* If compaction failed or was aborted without reporting a new
             context size, keep the previous one so auto-compaction retries
             on the next turn instead of forgetting it is over threshold. *)
          (match Composer_recorder.stop recorder with
           | Some (Failed _ | Interrupted) | None -> fst t.context
           | Some Done -> None))
     | Prompt | Autonomous ->
       Option.first_some
         tokens
         (let estimate = Message.estimate_tokens (Session.messages s.session) in
          match Composer_recorder.last_input recorder with
          | Some input -> Some (Int.min input estimate)
          | None -> Some estimate)
   in
   t.context <- tokens, Option.first_some window old_window);
  let cost = Composer_recorder.cost_usd recorder in
  if Float.( > ) cost 0. then s.add_spend ~usage:Usage.zero ~cost_usd:cost;
  t.services <- None;
  let added = Composer_recorder.finish recorder in
  (* What the harness saw, for a later handoff to give it only what came
     after. *)
  (match t.thread_id, Session.head s.session with
   | Some thread_id, Some through when !sent ->
     t.turns <- t.turns + 1;
     let turn_id =
       Option.value !last_turn ~default:(sprintf "turn-%d" t.turns)
     in
     let after =
       if Option.is_some !before then capture t s ~which:"after" else None
     in
     ignore
       (Session.record_external_turn
          s.session
          { thread_id
          ; turn_id
          ; through
          ; first_turn_id =
              Option.filter !first_turn ~f:(fun f ->
                not (String.equal f turn_id))
          ; mark = !mark
          ; before = Option.bind after ~f:(fun _ -> !before)
          ; after
          ; cost_total = Option.bind !cost_total ~f:(fun f -> f ())
          }
        : Session.Entry.t)
   | _ -> ());
  List.drop added (List.length input.prompts)
;;

(* Before the session is rewound to [to_]: the files go back to how they
   were before the first run it drops, and the agent (stopped here) drops
   those runs from its thread when it next starts ([rollback_plan]). Refused,
   changing nothing, unless every run dropped kept checkpoints and no file
   changed outside them since: between them, or after the last. *)
let rewind t session ~to_ =
  let path = List.map (Session.active_path session) ~f:(fun e -> e.id) in
  let index id =
    Option.map (List.findi path ~f:(fun _ x -> String.equal x id)) ~f:fst
  in
  match index to_ with
  | None -> Ok ()
  | Some cut ->
    let dropped =
      List.filter (Session.external_turns session) ~f:(fun run ->
        match index run.through with
        | Some i -> i > cut
        | None -> false)
    in
    let (module A : Adapter.S) = t.adapter in
    let fork_instead = "fork from that message instead (/fork)" in
    let restore exec =
      let open Or_error.Let_syntax in
      let%bind checkpoints =
        List.map dropped ~f:(fun (run : Session.External_turn.t) ->
          match run.before, run.after with
          | Some before, Some after -> Ok (before, after)
          | _ ->
            Or_error.errorf
              "the files as %s left them after a turn being rewound were not \
               saved (%s), so prigh can't put them back: %s"
              A.display_name
              (match t.not_git with
               | Some (_, why) -> why
               | None -> "no checkpoint was taken")
              fork_instead)
        |> Or_error.all
      in
      let%bind () =
        List.zip_exn (List.drop_last_exn checkpoints) (List.tl_exn checkpoints)
        |> List.map ~f:(fun ((_, after), (before, _)) ->
          match%bind
            Checkpoint.Client.changed exec ~from:after ~to_:(Some before)
          with
          | [] -> Ok ()
          | files ->
            Or_error.errorf
              "files changed between %s's turns being rewound, outside them: \
               %s. Rewinding would undo those changes too: commit or stash \
               them first, or %s"
              A.display_name
              (String.concat ~sep:", " files)
              fork_instead)
        |> Or_error.all_unit
      in
      let before, _ = List.hd_exn checkpoints in
      let _, after = List.last_exn checkpoints in
      match Checkpoint.Client.restore exec ~to_:before ~expect:after with
      | Ok (_ : string list) -> Ok ()
      | Error e ->
        Or_error.errorf
          "could not put the files back as they were before %s's turn (nothing \
           was changed): %s"
          A.display_name
          (Error.to_string_hum e)
    in
    if List.is_empty dropped
    then Ok ()
    else (
      match t.checkpoint, A.rollback with
      | None, _ | _, Unsupported ->
        Or_error.errorf
          "%s sessions can't rewind: %s"
          A.display_name
          fork_instead
      | Some exec, (Revert | Resume_at) ->
        let%map.Or_error () = restore exec in
        if
          List.exists dropped ~f:(fun run ->
            Option.equal String.equal (Some run.thread_id) t.thread_id)
        then close t)
;;

(* Rewinding needs checkpoints of the files, which need a git repository. *)
let with_rewind t (c : Capabilities.t) =
  let (module A : Adapter.S) = t.adapter in
  match A.rollback, t.checkpoint, t.not_git with
  | Unsupported, _, _ -> c
  | _, None, _ -> { c with rewind = Unsupported }
  | _, Some _, Some (_, why) ->
    { c with
      rewind = Unsupported
    ; rewind_note =
        Some
          (sprintf
             "%s, so prigh can't put its files back: fork from that message \
              instead (/fork)"
             why)
    }
  | _, Some _, None -> c
;;

module R = struct
  type nonrec t = t

  let engine = Engine_id.Composer

  let capabilities t =
    with_rewind
      t
      (match t.live with
       | Some (Live ((module L), a)) -> L.current_capabilities a
       | None ->
         let (module A : Adapter.S) = t.adapter in
         Option.value t.known ~default:A.capabilities)
  ;;

  let models t =
    let (module A : Adapter.S) = t.adapter in
    (match t.live with
     | Some (Live ((module L), a)) ->
       (match L.models a with
        | [] -> ()
        | models -> t.models <- models)
     | None -> ());
    Some
      (match t.models with
       | _ :: _ as models -> models
       | [] ->
         (match t.catalog () with
          | Some { models = _ :: _ as models; _ } -> models
          | Some { models = []; _ } -> [ External_model.unlisted ~adapter:A.id ]
          | None -> [ External_model.pending ~adapter:A.id ]))
  ;;

  let context t = t.context
  let run_turn = run_turn
end

(* The login's limits as the running harness said last, else as its
   catalog did. *)
let limits t =
  let live =
    match t.live with
    | Some (Live ((module L), a)) -> L.limits a
    | None -> None
  in
  match live with
  | Some _ -> live
  | None -> Option.bind (t.catalog ()) ~f:(fun c -> c.limits)
;;

(* prigh compacted the session ([compacted_since]): the process stops, and
   the next turn starts a new thread, its context not yet known. *)
let compacted t =
  close t;
  t.context <- None, snd t.context
;;

let runner t =
  Runner.create
    ~close:(fun () -> close t)
    ~compacted:(fun () -> compacted t)
    ~rewind:(rewind t)
    ~limits:(fun () -> limits t)
    ~prewarm:(prewarm t)
    ~live:(fun () ->
      Option.is_some t.warming
      ||
      match t.live with
      | Some (Live ((module L), a)) -> L.alive a
      | None -> false)
    (module R)
    t
;;
