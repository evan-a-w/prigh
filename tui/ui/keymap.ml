open! Core

module Binding = struct
  type t =
    { keys : Key.t list
    ; intent : Intent.t
    ; help : string
    }
end

let b keys intent help = { Binding.keys; intent; help }

let bindings =
  [ b [ Key.plain Enter ] Submit "send the prompt / accept the highlighted item"
  ; b [ Key.alt Enter; Key.ctrl 'j' ] Newline "insert a newline in the editor"
  ; b [ Key.plain Escape ] Cancel "close the dialog, or abort the running turn"
  ; b
      [ Key.plain Tab ]
      Complete
      "complete a slash command / open the command picker"
  ; b [ Key.plain Up ] Up "move up (editor line, history, or list row)"
  ; b [ Key.plain Down ] Down "move down (editor line, history, or list row)"
  ; b [ Key.plain Left ] Left "move the cursor left"
  ; b [ Key.plain Right ] Right "move the cursor right"
  ; b [ Key.plain Home; Key.ctrl 'a' ] Home "start of line"
  ; b [ Key.plain End; Key.ctrl 'e' ] End "end of line"
  ; b [ Key.plain Page_up ] Page_up "scroll the transcript / list up a page"
  ; b
      [ Key.plain Page_down ]
      Page_down
      "scroll the transcript / list down a page"
  ; b
      [ Key.plain Backspace; Key.ctrl 'h' ]
      Backspace
      "delete the character before the cursor"
  ; b [ Key.plain Delete ] Delete "delete the character under the cursor"
  ; b [ Key.ctrl 'k' ] Kill_to_end "delete to end of line"
  ; b [ Key.ctrl 'u' ] Kill_line "delete the whole line"
  ; b [ Key.ctrl 'w' ] Kill_word "delete the word before the cursor"
  ; b [ Key.ctrl 'l' ] Clear_screen "clear the transcript"
  ; b
      [ Key.ctrl 'o' ]
      Cycle_verbosity
      "cycle transcript verbosity (quiet / normal / verbose)"
  ; b
      [ { (Key.plain Tab) with shift = true } ]
      Next_agent
      "cycle focus: main → agent 1 → … → main"
  ; b [ Key.alt (Char "1") ] (Focus_agent 1) "focus agent N (Alt+1…9)"
  ; b [ Key.ctrl 'c' ] Interrupt "clear the editor, then (again) quit"
  ; b [ Key.ctrl 'd' ] Force_quit "quit"
  ]
;;

let table =
  List.concat_map bindings ~f:(fun b ->
    List.map b.keys ~f:(fun k -> k, b.intent))
;;

let lookup (key : Key.t) =
  match List.Assoc.find table ~equal:Key.equal key with
  | Some intent -> Some intent
  | None ->
    (match key.code with
     | Char c when (not key.ctrl) && not key.alt -> Some (Intent.Insert c)
     | Char c when key.alt && not key.ctrl ->
       (match Int.of_string_opt c with
        | Some n when n >= 1 && n <= 9 -> Some (Intent.Focus_agent n)
        | _ -> None)
     | _ -> None)
;;

let help : Content.t =
  let rows =
    List.map bindings ~f:(fun b ->
      String.concat ~sep:" / " (List.map b.keys ~f:Key.to_string), b.help)
  in
  let width =
    List.fold rows ~init:0 ~f:(fun acc (k, _) -> Int.max acc (String.length k))
  in
  List.map rows ~f:(fun (k, help) ->
    [ { Content.Span.text = Text_width.pad_right k ~width
      ; style = Style.bold Style.plain
      }
    ; { text = "  " ^ help; style = Style.plain }
    ])
;;
