open! Core
open! Import

let of_session (m : App.Model.t) (s : Session_summary.t) =
  let cwd =
    (* The current session's as it is now: [/cd] moves it. *)
    match m.state with
    | Some state when String.equal state.session_id s.id -> state.cwd
    | _ -> s.cwd
  in
  Prigh_ui.Session_worktrees.of_session
    ~listings:
      (Option.to_list (Worktrees.listing m.worktrees)
       @ Repo_listings.listings m.repo_worktrees)
    ~unsettled:(Repo_listings.unsettled m.repo_worktrees)
    ~cwd
    s
;;
