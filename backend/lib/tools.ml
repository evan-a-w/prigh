open! Core
open! Import

let all =
  [ Tool_bash.tool
  ; Tool_read.tool
  ; Tool_write.tool
  ; Tool_edit.tool
  ; Tool_ls.tool
  ; Tool_grep.tool
  ; Tool_find.tool
  ]
;;

let find name = List.find all ~f:(fun t -> String.equal (Tool.name t) name)
let specs tools = List.map tools ~f:(fun (t : Tool.t) -> t.spec)
