open! Core

let href id = "?session=" ^ id

let markdown ~id ~title =
  let title =
    String.map title ~f:(function
      | '[' -> '('
      | ']' -> ')'
      | '\n' | '\r' | '\t' -> ' '
      | c -> c)
  in
  sprintf "[%s](%s)" title (href id)
;;
