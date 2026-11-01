open! Core
open! Import

let changes_work ~tool args =
  let action = Tool_args.string_opt args "action" in
  if Approval.is_edit tool
  then true
  else if String.equal tool Tool_worktree.name
  then not ([%equal: string option] action (Some "list"))
  else if String.equal tool Tool_pull_request.name
  then Option.value_map action ~default:true ~f:Tool_pull_request.changes
  else if String.equal tool Tool_issue.name
  then Option.value_map action ~default:true ~f:Tool_issue.changes
  else if String.equal tool Tool_repo.name
  then Option.value_map action ~default:true ~f:Tool_repo.changes
  else if String.equal tool Tool_subagent.name
  then Tool_subagent.wants_worktree args
  else false
;;
