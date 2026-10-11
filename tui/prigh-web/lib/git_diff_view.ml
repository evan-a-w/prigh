open! Core
open! Import
open Html
module Diff_files = Prigh_ui.Diff_files

let file ~fold (f : Diff_files.File.t) =
  let status =
    match f.status with
    | Added -> Some "new"
    | Deleted -> Some "deleted"
    | Binary -> Some "binary"
    | Modified -> None
  in
  Node.details
    ~attrs:[ Attr.class_ "diff-file"; Attr.bool_property "open" true ]
    [ Node.summary
        (List.filter_opt
           [ Some (span ~cls:"path" f.path)
           ; Option.map status ~f:(span ~cls:"chip")
           ; Some (span ~cls:"count add" (sprintf "+%d" f.added))
           ; Some (span ~cls:"count del" (sprintf "−%d" f.removed))
           ])
    ; (match f.hunks with
       | [] -> div ~cls:"diff-empty" [ Node.text "No text changes to show." ]
       | hunks ->
         Diff_view.unified
           ~max_rows:400
           ~fold:(fold, f.path)
           (String.concat ~sep:"\n" hunks))
    ]
;;

let view ~fold (d : Git_diff.t) ~inject =
  let files = Diff_files.parse d.diff in
  Modal.view
    ~cls:"diff-dialog"
    ~title:"Changes"
    ~on_close:(inject App.Action.Close_dialog)
    ~footer:
      [ span ~cls:"keys-hint" "/diff PATH shows one file or directory"
      ; button
          ~cls:"primary"
          ~on_click:(inject App.Action.Close_dialog)
          [ Node.text "Done" ]
      ]
    ([ Node.p
         ~attrs:[ Attr.class_ "dialog-note" ]
         [ span ~cls:"root" d.root
         ; Node.text (" against " ^ Diff_files.against d ^ "  ")
         ; span ~cls:"summary" (Diff_files.summary files)
         ]
     ]
     @ List.map files ~f:(file ~fold:(Fold.scoped fold "git-diff"))
     @
     if d.truncated
     then
       [ Node.p
           ~attrs:[ Attr.class_ "dialog-note warn" ]
           [ Node.text
               "The diff is cut short at 1 MB: /diff PATH shows part of it."
           ]
       ]
     else [])
;;
