open! Core
open! Import

let pattern =
  Re.compile (Re.Perl.re {|\bgit\b.*\bworktree\s+(add|remove|move|prune)\b|})
;;

let applies command = Re.execp pattern command

let listing (context : Tool.Context.t) ~dir =
  Tool.execute_via
    { context with on_output = ignore }
    Worktree.list_tool
    (Worktree.list_args ~dir)
  |> Worktree.listing_of_result
  |> Result.ok
;;

let note ~(listing : Worktree.Listing.t) = function
  | [] -> ""
  | (outside : Worktree.t list) ->
    sprintf
      "\n\
       [prigh: git worktree add made %s outside the standard place (%s). It is \
       recorded on the session, but next time use the worktree tool (or the \
       subagent tool's worktree option) rather than git worktree add.]"
      (String.concat
         ~sep:", "
         (List.map outside ~f:(fun (w : Worktree.t) -> w.path)))
      listing.project_dir
;;

let around (context : Tool.Context.t) ~dir ~arguments run =
  match Json.member "command" arguments with
  | Some (`String command) when applies command ->
    let before = listing context ~dir in
    let (result : Tool_result.t) = run () in
    (match before, listing context ~dir with
     | Some before, Some after ->
       let paths (l : Worktree.Listing.t) =
         List.map l.worktrees ~f:(fun (w : Worktree.t) -> w.path)
       in
       let mem l path = List.mem (paths l) path ~equal:String.equal in
       let added =
         List.filter after.worktrees ~f:(fun (w : Worktree.t) ->
           (not w.main) && not (mem before w.path))
       in
       let removed =
         List.filter (paths before) ~f:(fun path -> not (mem after path))
       in
       if not (List.is_empty added && List.is_empty removed)
       then
         context.emit
           (Worktrees_changed
              { added =
                  List.map added ~f:(fun (w : Worktree.t) ->
                    { Session_worktree.path = w.path
                    ; branch = w.branch
                    ; root = after.root
                    ; source = Created
                    })
              ; removed
              });
       { result with
         text =
           result.text
           ^ note
               ~listing:after
               (List.filter added ~f:(fun (w : Worktree.t) -> not w.managed))
       }
     | _ -> result)
  | _ -> run ()
;;
