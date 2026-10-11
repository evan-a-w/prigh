open! Core

(* Control characters would end the sequence early; [;] separates OSC 777's
   fields. *)
let clean ?(semicolons = false) s =
  String.map s ~f:(fun c ->
    if Char.is_print c && not (semicolons && Char.equal c ';') then c else ' ')
;;

let osc777 term =
  match term with
  | Some term ->
    List.exists [ "foot"; "rxvt" ] ~f:(fun prefix ->
      String.is_prefix term ~prefix)
  | None -> false
;;

let tmux_passthrough s =
  "\027Ptmux;" ^ String.substr_replace_all s ~pattern:"\027" ~with_:"\027\027" ^ "\027\\"
;;

let sequence (mode : Prigh_ui.Notify.Mode.t) ~term ~tmux (n : Prigh_ui.Notify.t) =
  match mode with
  | Off -> ""
  | Bell -> "\007"
  | Desktop ->
    let osc =
      if osc777 term
      then
        sprintf
          "\027]777;notify;%s;%s\027\\"
          (clean ~semicolons:true n.title)
          (clean ~semicolons:true n.body)
      else sprintf "\027]9;%s: %s\007" (clean n.title) (clean n.body)
    in
    if tmux then tmux_passthrough osc else osc
;;
