open! Core
open! Import

let environment ~extra =
  let overridden = String.Set.of_list (List.map extra ~f:fst) in
  let inherited =
    Array.to_list (Core_unix.environment ())
    |> List.filter ~f:(fun entry ->
      match String.lsplit2 entry ~on:'=' with
      | Some (key, _) -> not (Set.mem overridden key)
      | None -> true)
  in
  inherited @ List.map extra ~f:(fun (k, v) -> k ^ "=" ^ v)
;;

let is_executable path =
  match Core_unix.access path [ `Exec ] with
  | Ok () -> not (Sys_unix.is_directory_exn path)
  | Error _ -> false
;;

module Missing = struct
  type t =
    | Not_executable of string
    | Not_on_path
  [@@deriving sexp_of]
end

let resolve ~environment ~dir command =
  if String.mem command '/'
  then (
    let path =
      if Filename.is_relative command
      then Filename.concat dir command
      else command
    in
    if is_executable path then Ok path else Error (Missing.Not_executable path))
  else (
    let path =
      List.find_map environment ~f:(String.chop_prefix ~prefix:"PATH=")
      |> Option.value ~default:"/usr/bin:/bin"
    in
    (* Only absolute entries: a relative one (or an empty one, meaning ".")
       would be looked up in one directory and run in another, the
       session's, which may be an untrusted repository. *)
    List.find_map (String.split path ~on:':') ~f:(fun entry ->
      if Filename.is_relative entry
      then None
      else (
        let candidate = Filename.concat entry command in
        Option.some_if (is_executable candidate) candidate))
    |> Result.of_option ~error:Missing.Not_on_path)
;;

let login_shell_path ~env name =
  let output =
    Process.run_collect
      ~env
      ~timeout:(Time_ns.Span.of_sec 3.)
      ~prog:"bash"
      ~args:[ "-lc"; {|command -v -- "$1"|}; "bash"; name ]
      ()
  in
  match output.exit with
  | Exited 0 ->
    (match String.split_lines output.stdout |> List.last with
     | Some line ->
       let path = String.strip line in
       Option.some_if
         ((not (Filename.is_relative path)) && is_executable path)
         path
     | None -> None)
  | Exited _ | Signaled _ | Timed_out | Cancelled -> None
  | exception _ -> None
;;

let describe_status = function
  | `Exited n -> sprintf "exited (code %d)" n
  | `Signaled n ->
    sprintf "was killed (%s)" (Signal.to_string (Signal.of_caml_int n))
;;
