open! Core
open! Import

let usage =
  "/adapters [refresh | retry] | /adapters login [ADAPTER] [LOGIN|own] | /adapters \
   import ADAPTER [HOST]"
;;

module Action = struct
  type t =
    | List of { refresh : bool }
    | Retry
    | Login of
        { adapter : string option
        ; login : string option
        }
    | Import of
        { adapter : string
        ; host : string option
        }
  [@@deriving sexp_of, equal]
end

(* What the user may type for an adapter whose id is longer. *)
let canonical adapter =
  match String.lowercase adapter with
  | "antigravity" | "agy" -> "acp:antigravity"
  | _ -> adapter
;;

let parse rest =
  match
    String.split (String.strip rest) ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [] -> Ok (Action.List { refresh = false })
  | [ "refresh" ] -> Ok (List { refresh = true })
  | [ "retry" ] -> Ok Retry
  | [ "login" ] -> Ok (Login { adapter = None; login = None })
  | [ "login"; adapter ] ->
    Ok (Login { adapter = Some (canonical adapter); login = None })
  | [ "import" ] ->
    Or_error.error_string
      "which CLI? /adapters import codex|claude|antigravity [HOST] moves its \
       login here"
  | [ "import"; adapter ] ->
    Ok (Import { adapter = canonical adapter; host = None })
  | "import" :: adapter :: host ->
    Ok
      (Import
         { adapter = canonical adapter
         ; host = Some (String.concat ~sep:" " host)
         })
  | "login" :: adapter :: login ->
    Ok
      (Login
         { adapter = Some (canonical adapter)
         ; login = Some (String.concat ~sep:" " login)
         })
  | _ -> Or_error.errorf "unknown argument %S: %s" (String.strip rest) usage
;;

let login_lines (logins : P.Adapter_login.t list) =
  match logins with
  | [] -> []
  | _ ->
    ""
    :: "Logins:"
    :: List.map logins ~f:(fun (l : P.Adapter_login.t) ->
      sprintf
        "  %s runs on %s  (/adapters login %s [LOGIN|own], /adapters import %s)"
        (P.Session_engine.adapter_label l.adapter)
        (P.Adapter_login.describe l)
        l.adapter
        l.adapter)
;;

let lines (statuses : P.Adapter_status.t list) logins =
  (match statuses with
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
         let mark =
           match s.problem, s.auth with
           | Some _, _ -> "✗"
           | None, Unknown when s.checking || s.checkable -> "?"
           | None, _ -> "✓"
         in
         sprintf "  %s %s  %s" mark s.label (P.Adapter_status.summary s)
         :: List.map
              (Option.to_list (Option.first_some s.problem s.hint))
              ~f:(fun p -> "      " ^ p)))
     @ [ ""; "prigh never installs them or logs in: run that on the host" ])
  @ login_lines logins
;;

let find_login (logins : P.Adapter_login.t list) ~adapter =
  let names () =
    String.concat ~sep:", " (List.map logins ~f:(fun l -> l.adapter))
  in
  match adapter, logins with
  | _, [] ->
    Or_error.error_string
      "no agent CLI here can run on prigh's logins: each uses its own"
  | None, [ l ] -> Ok l
  | None, _ ->
    Or_error.errorf "which CLI? /adapters login ADAPTER (%s)" (names ())
  | Some a, _ ->
    (match
       List.find logins ~f:(fun (l : P.Adapter_login.t) ->
         String.Caseless.equal l.adapter a
         || String.Caseless.equal (P.Session_engine.adapter_label l.adapter) a)
     with
     | Some l -> Ok l
     | None ->
       Or_error.errorf
         "%s runs only on its own login; prigh can hand one of its logins to: \
          %s"
         a
         (names ()))
;;

let changed (l : P.Adapter_login.t) =
  sprintf
    "%s runs on %s from its next turn%s"
    (P.Session_engine.adapter_label l.adapter)
    (P.Adapter_login.describe l)
    (match l.login, P.Adapter_login.warning l with
     | Some _, Some warning -> " (" ^ warning ^ ")"
     | _ -> "")
;;
