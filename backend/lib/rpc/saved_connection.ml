open! Core
open! Import

type t =
  { connect : string
  ; token : string option [@sexp.option]
  ; user : string option [@sexp.option]
  }
[@@deriving sexp, equal]

let file ~home = home ^/ ".prigh" ^/ "tui-connection.sexp"

let load ~home =
  let path = file ~home in
  match Sys_unix.file_exists_exn path with
  | false -> Ok None
  | true ->
    Or_error.try_with (fun () -> Some (Sexp.load_sexp_conv_exn path t_of_sexp))
    |> Or_error.tag ~tag:path
;;

let save ~home t =
  let path = file ~home in
  Or_error.try_with (fun () ->
    Core_unix.mkdir_p (Filename.dirname path);
    let tmp = sprintf "%s.%s.tmp" path (Pid.to_string (Core_unix.getpid ())) in
    Out_channel.with_file tmp ~perm:0o600 ~f:(fun oc ->
      Out_channel.output_string oc (Sexp.to_string_hum (sexp_of_t t) ^ "\n"));
    Core_unix.chmod tmp ~perm:0o600;
    Core_unix.rename ~src:tmp ~dst:path)
  |> Or_error.tag ~tag:path
;;

let forget ~home =
  let path = file ~home in
  Or_error.try_with (fun () ->
    match Core_unix.unlink path with
    | () -> ()
    | exception Core_unix.Unix_error (ENOENT, _, _) -> ())
  |> Or_error.tag ~tag:path
;;

module Settings = struct
  type t =
    { connect : string option
    ; token : string option
    ; user : string option
    }
  [@@deriving sexp_of]
end

module Resolved = struct
  type nonrec t =
    { settings : Settings.t
    ; save : t option
    }
  [@@deriving sexp_of]
end

let resolve ~saved ~(flags : Settings.t) ~(env : Settings.t) ~local : Resolved.t
  =
  if local
  then
    { settings =
        { connect = None
        ; token = Option.first_some flags.token env.token
        ; user = Option.first_some flags.user env.user
        }
    ; save = None
    }
  else (
    let connect =
      Option.first_some
        flags.connect
        (Option.first_some
           env.connect
           (Option.map saved ~f:(fun (s : t) -> s.connect)))
    in
    let same =
      Option.filter saved ~f:(fun (s : t) ->
        Option.equal String.equal connect (Some s.connect))
    in
    let from_same f = Option.bind same ~f in
    let token = Option.first_some flags.token (from_same (fun s -> s.token)) in
    let user = Option.first_some flags.user (from_same (fun s -> s.user)) in
    let flagged =
      Option.is_some flags.connect
      || Option.is_some flags.token
      || Option.is_some flags.user
    in
    let save =
      match connect with
      | Some connect when flagged ->
        let t = { connect; token; user } in
        if Option.equal equal (Some t) saved then None else Some t
      | _ -> None
    in
    { settings =
        { connect
        ; token =
            Option.first_some flags.token (Option.first_some env.token token)
        ; user = Option.first_some flags.user (Option.first_some env.user user)
        }
    ; save
    })
;;
