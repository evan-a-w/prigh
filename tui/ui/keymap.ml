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
  ; b
      [ Key.alt Enter ]
      Queue_follow_up
      "queue a follow-up to run after the current turn"
  ; b [ Key.ctrl 'j'; Key.alt (Char "j") ] Newline "insert a newline"
  ; b [ Key.plain Escape ] Cancel "close the dialog, or abort the running turn"
  ; b
      [ Key.plain Tab ]
      Complete
      "complete a slash command / open the command picker"
  ; b [ Key.plain Up ] Up "move up (editor line, history, or list row)"
  ; b [ Key.plain Down ] Down "move down (editor line, history, or list row)"
  ; b
      [ Key.alt Up ]
      Dequeue
      "pop the last queued steer/follow-up back into the editor"
  ; b [ Key.plain Left ] Left "move the cursor left"
  ; b [ Key.plain Right ] Right "move the cursor right"
  ; b
      [ Key.alt (Char "b"); { (Key.plain Left) with ctrl = true } ]
      Word_left
      "move the cursor back one word"
  ; b
      [ Key.alt (Char "f"); { (Key.plain Right) with ctrl = true } ]
      Word_right
      "move the cursor forward one word"
  ; b [ Key.alt (Char "d") ] Delete_word_forward "delete the next word"
  ; b [ Key.plain Home; Key.ctrl 'a' ] Home "start of line"
  ; b [ Key.plain End; Key.ctrl 'e' ] End "end of line"
  ; b [ Key.plain Page_up ] Page_up "scroll the transcript / list up a page"
  ; b
      [ Key.plain Page_down ]
      Page_down
      "scroll the transcript / list down a page"
  ; b
      [ { (Key.plain Up) with ctrl = true } ]
      Prev_user_message
      "jump to the previous user message"
  ; b
      [ { (Key.plain Down) with ctrl = true } ]
      Next_user_message
      "jump to the next user message"
  ; b
      [ Key.plain Backspace; Key.ctrl 'h' ]
      Backspace
      "delete the character before the cursor"
  ; b [ Key.plain Delete ] Delete "delete the character under the cursor"
  ; b [ Key.ctrl 'k' ] Kill_to_end "delete to the end of the line"
  ; b [ Key.ctrl 'u' ] Kill_to_start "delete to the start of the line"
  ; b
      [ Key.ctrl 'w'; Key.alt Backspace ]
      Kill_word
      "delete the word before the cursor"
  ; b [ Key.ctrl 'y' ] Yank "paste the most recent kill"
  ; b [ Key.alt (Char "y") ] Yank_pop "replace the last yank with an older kill"
  ; b [ Key.ctrl '_' ] Undo "undo the last edit"
  ; b [ Key.ctrl 'o' ] Cycle_verbosity "cycle transcript verbosity"
  ; b [ Key.ctrl 'r' ] Path_complete "complete a file path at the cursor"
  ; b [ Key.ctrl 'f' ] Search "search the transcript"
  ; b [ Key.ctrl 'g' ] Edit_externally "edit the prompt in $EDITOR"
  ; b [ Key.ctrl 'l' ] Model_picker "pick a model"
  ; b
      [ Key.ctrl 'p' ]
      Next_model
      "cycle to the next scoped model (Shift+Ctrl+P is unavailable; Alt+P goes \
       back)"
  ; b [ Key.alt (Char "p") ] Prev_model "cycle to the previous scoped model"
  ; b [ Key.ctrl 't' ] Next_thinking "cycle the thinking level"
  ; b
      [ Key.ctrl 'n' ]
      Picker_toggle_filter
      "picker: toggle the named-only / logged-in-only filter"
  ; b [ Key.ctrl 'x' ] Copy_last "copy the last assistant message"
  ; b [ Key.ctrl 'z' ] Suspend "suspend to the shell"
  ; b
      [ { (Key.plain Tab) with shift = true } ]
      Next_agent
      "cycle focus: main → agent 1 → … → main"
  ; b [ Key.alt (Char "1") ] (Focus_agent 1) "focus agent N (Alt+1…9)"
  ; b
      [ Key.ctrl 'c' ]
      Interrupt
      "clear the editor or abort the turn, then (again) quit"
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
