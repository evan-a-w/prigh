open! Core
open! Import

type t =
  { spec : Tool_spec.t
  ; run : context -> Json.t -> Tool_result.t
  }

and context =
  { env : Env.t
  ; cwd : string
  ; cancel : Cancellation.t
  ; on_output : string -> unit
  ; depth : int
  ; agent_id : string option
  ; call_id : string
  ; tools : t list
  ; emit : Agent_event.t -> unit
  ; execute : executor
  }

and executor = context -> t -> Json.t -> Tool_result.t

module Result = Tool_result

let name t = t.spec.name

let execute t context args =
  match t.run context args with
  | result -> result
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
        ?(emit = ignore)
        ~env
        ~cwd
        ()
    =
    { env
    ; cwd
    ; cancel
    ; on_output
    ; depth
    ; agent_id
    ; call_id
    ; tools
    ; emit
    ; execute
    }
  ;;
end

let expand_home path =
  let home = Option.value (Sys.getenv "HOME") ~default:"/" in
  match String.chop_prefix path ~prefix:"~/" with
  | Some rest -> Filename.concat home rest
  | None -> if String.equal path "~" then home else path
;;

let resolve ~cwd path =
  let path = expand_home path in
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

let resolve_path (context : Context.t) path = resolve ~cwd:context.cwd path
