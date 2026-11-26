open! Core
open! Import

module Status = struct
  type t =
    | Works
    | Unavailable of string
    | Unknown of string
  [@@deriving sexp_of, equal]
end

let request : Host_probe.Request.t =
  { programs =
      [ { name = "codex"; version = [ "sandbox"; "--"; "true" ]; status = [] } ]
  ; paths = []
  ; env = []
  }
;;

(* What bubblewrap (or Landlock, Seatbelt) says when it cannot set the
   sandbox up, rather than Codex not knowing [sandbox]. *)
let sandbox_failures =
  [ "namespace"
  ; "bwrap"
  ; "bubblewrap"
  ; "landlock"
  ; "seccomp"
  ; "operation not permitted"
  ; "permission denied"
  ]
;;

let first_line text =
  String.split_lines text
  |> List.map ~f:String.strip
  (* Codex's own warnings, e.g. about its helper binaries *)
  |> List.find ~f:(fun l ->
    (not (String.is_empty l)) && not (String.is_prefix l ~prefix:"WARNING"))
  |> Option.map ~f:(fun l -> String.prefix l 160)
;;

let of_output (out : Host_probe.Command_output.t) : Status.t =
  let text = out.stderr ^ "\n" ^ out.stdout in
  let lower = String.lowercase text in
  let has substring = String.is_substring lower ~substring in
  match out.exit with
  | Exited 0 -> Works
  | Exited _ when List.exists sandbox_failures ~f:has ->
    Unavailable
      (if has "namespace"
       then "no user namespaces"
       else Option.value (first_line text) ~default:"it failed")
  | Exited code ->
    Unknown
      (Option.value (first_line text) ~default:(sprintf "it exited %d" code))
  | Signaled _ -> Unknown "it was killed"
  | Timed_out -> Unknown "it did not finish in time"
  | Cancelled -> Unknown "cancelled"
;;

let of_result result =
  match Host_probe.Reply.of_result result with
  | Error e -> None, Status.Unknown (Error.to_string_hum e)
  | Ok reply ->
    (match reply.programs with
     | { path = Some path; version = Some out; _ } :: _ ->
       Some path, of_output out
     | { path = Some path; version = None; _ } :: _ ->
       Some path, Unknown "it did not run"
     | { path = None; _ } :: _ | [] -> None, Unknown "codex is not installed")
;;

module Cache = struct
  type t = (string * Status.t) String.Table.t

  let create () = String.Table.create ()

  let status t ~host ~installed ~exec =
    match Hashtbl.find t host with
    | Some (path, status)
      when Option.value_map installed ~default:true ~f:(String.equal path) ->
      status
    | Some _ | None ->
      let path, status =
        of_result
          (exec ~name:Host_probe.op (Host_probe.Request.to_json request))
      in
      Option.iter path ~f:(fun path ->
        Hashtbl.set t ~key:host ~data:(path, status));
      status
  ;;

  let find t ~host = Option.map (Hashtbl.find t host) ~f:snd
  let forget_host t ~host = Hashtbl.remove t host
end
