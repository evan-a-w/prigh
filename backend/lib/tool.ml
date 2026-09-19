open! Core
open! Import

module Context = struct
  type t =
    { env : Env.t
    ; cwd : string
    ; cancel : Cancellation.t
    ; on_output : string -> unit
    }

  let create ?(cancel = Cancellation.never) ?(on_output = ignore) ~env ~cwd () =
    { env; cwd; cancel; on_output }
  ;;
end

module Result = struct
  type t =
    { text : string
    ; is_error : bool
    }
  [@@deriving sexp_of]

  let ok text = { text; is_error = false }
  let error text = { text; is_error = true }
end

type t =
  { spec : Tool_spec.t
  ; run : Context.t -> Json.t -> Result.t
  }

let name t = t.spec.name

let execute t context args =
  match t.run context args with
  | result -> result
  | exception Tool_args.Invalid msg -> Result.error ("invalid arguments: " ^ msg)
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    Result.error (sprintf "%s failed: %s" t.spec.name (Exn.to_string exn))
;;

let expand_home path =
  let home = Option.value (Sys.getenv "HOME") ~default:"/" in
  match String.chop_prefix path ~prefix:"~/" with
  | Some rest -> Filename.concat home rest
  | None -> if String.equal path "~" then home else path
;;

let resolve_path (context : Context.t) path =
  let path = expand_home path in
  let path =
    if Filename.is_absolute path then path else Filename.concat context.cwd path
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
