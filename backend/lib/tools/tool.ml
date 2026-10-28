open! Core
open! Import

type t =
  { spec : Tool_spec.t
  ; run : context -> Json.t -> Tool_result.t
  }

and context =
  { env : Env.t
  ; cwd : string
  ; home : unit -> string option
  ; cancel : Cancellation.t
  ; on_output : string -> unit
  ; depth : int
  ; agent_id : string option
  ; call_id : string
  ; tools : t list
  ; conversation : Conversation.t option
  ; emit : Agent_event.t -> unit
  ; background : Background_tasks.t option
  ; execute : executor
  ; cache_key : string option
  ; halt : unit -> string option
  ; stdin : Stdin_feed.t option
  ; files : File_access.t
  ; approve : approver
  ; ask_user :
      call_id:string
      -> timeout_seconds:int option
      -> User_question.t list
      -> User_question.Reply.t
  }

and executor = context -> t -> Json.t -> Tool_result.t
and approver = context -> t -> Json.t -> Tool_result.t option

module Result = Tool_result

let name t = t.spec.name

let execute t context args =
  match t.run context args with
  | result -> { result with text = Utf8.sanitize result.text }
  | exception Tool_args.Invalid msg -> Result.error ("invalid arguments: " ^ msg)
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    Result.error (sprintf "%s failed: %s" t.spec.name (Exn.to_string exn))
;;

let local_executor context t args = execute t context args
let execute_via context t args = context.execute context t args

module Context = struct
  type nonrec t = context

  let create
        ?(cancel = Cancellation.never)
        ?(execute = local_executor)
        ?(on_output = ignore)
        ?(depth = 0)
        ?agent_id
        ?(call_id = "")
        ?(tools = [])
        ?conversation
        ?(emit = ignore)
        ?background
        ?cache_key
        ?(halt = fun () -> None)
        ?stdin
        ?(files = File_access.local)
        ?(approve = fun _ _ _ -> None)
        ?(home = fun () -> None)
        ?(ask_user =
          fun ~call_id:_ ~timeout_seconds:_ _ ->
            User_question.Reply.Unavailable "no user to ask")
        ~env
        ~cwd
        ()
    =
    { env
    ; cwd
    ; home
    ; cancel
    ; on_output
    ; depth
    ; agent_id
    ; call_id
    ; tools
    ; conversation
    ; emit
    ; background
    ; execute
    ; cache_key
    ; halt
    ; stdin
    ; files
    ; approve
    ; ask_user
    }
  ;;
end

let expand_home ?home path =
  let home =
    match home with
    | Some home -> home
    | None -> Option.value (Sys.getenv "HOME") ~default:"/"
  in
  match String.chop_prefix path ~prefix:"~/" with
  | Some rest -> Filename.concat home rest
  | None -> if String.equal path "~" then home else path
;;

let resolve ?home ~cwd path =
  let path = expand_home ?home path in
  let path =
    if Filename.is_absolute path then path else Filename.concat cwd path
  in
  (* Drop "." segments so that "." resolves to the cwd itself. *)
  let parts =
    List.filter (String.split path ~on:'/') ~f:(fun p ->
      not (String.equal p "."))
  in
  match parts with
  | [ "" ] | [] -> "/"
  | _ -> String.concat parts ~sep:"/"
;;

let resolve_path (context : Context.t) path =
  let home =
    if String.is_prefix path ~prefix:"~" then context.home () else None
  in
  resolve ?home ~cwd:context.cwd path
;;
