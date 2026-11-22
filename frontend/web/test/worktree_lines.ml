open! Core
module H = Harness
open Prigh_web

(* The worktrees each session shows (those its fold leaves open), one line
   a session, as the sidebar listed them. *)
let print h =
  let m = H.model h in
  let shown = App.Model.worktrees_shown m in
  List.iter m.sessions ~f:(fun s ->
    match Session_worktree_nodes.of_session m s with
    | [] -> ()
    | _ when s.archived || not (Set.mem shown s.id) -> ()
    | nodes ->
      List.map nodes ~f:(fun (n : Prigh_ui.Session_worktrees.Node.t) ->
        let w = n.worktree in
        let listed f = Option.bind n.listed ~f in
        [ Some (Prigh_ui.Session_worktrees.Node.label n)
        ; (match w.source with
           | Agent _ -> Some "subagent"
           | Pull_request number -> Some (sprintf "#%d" number)
           | Created | Entered -> None)
        ; Option.some_if n.here "here"
        ; listed (fun l ->
            match l.changes with
            | Some c when c > 0 -> Some (sprintf "● %d" c)
            | _ -> None)
        ; listed (fun l ->
            match
              List.filter_opt
                [ Option.some_if (l.ahead > 0) (sprintf "↑%d" l.ahead)
                ; Option.some_if (l.behind > 0) (sprintf "↓%d" l.behind)
                ]
            with
            | [] -> None
            | parts -> Some (String.concat ~sep:" " parts))
        ; listed (fun l -> Option.some_if l.locked "locked")
        ; listed (fun l -> Option.some_if l.prunable "gone")
        ]
        |> List.filter_opt
        |> String.concat ~sep:" "
        |> sprintf "(%s)")
      |> String.concat ~sep:" "
      |> print_endline)
;;
