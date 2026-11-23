open! Core
open! Import

let usage = "/adapters [refresh]"

let parse rest =
  match String.strip rest with
  | "" -> Ok false
  | "refresh" -> Ok true
  | other -> Or_error.errorf "unknown argument %S: %s" other usage
;;

let lines (statuses : P.Adapter_status.t list) =
  match statuses with
  | [] -> [ "no tool host is connected: the agent CLIs run on one" ]
  | _ ->
    let hosts =
      List.fold statuses ~init:[] ~f:(fun acc (s : P.Adapter_status.t) ->
        if List.mem acc s.host ~equal:String.equal then acc else s.host :: acc)
      |> List.rev
    in
    List.concat_map hosts ~f:(fun host ->
      let here =
        List.filter statuses ~f:(fun (s : P.Adapter_status.t) ->
          String.equal s.host host)
      in
      let name = (List.hd_exn here).host_name in
      (name ^ ":")
      :: List.concat_map here ~f:(fun (s : P.Adapter_status.t) ->
        let mark = if Option.is_some s.problem then "✗" else "✓" in
        sprintf "  %s %s  %s" mark s.label (P.Adapter_status.summary s)
        :: List.map (Option.to_list s.problem) ~f:(fun p -> "      " ^ p)))
    @ [ ""; "prigh never installs them or logs in: run that on the host" ]
;;
