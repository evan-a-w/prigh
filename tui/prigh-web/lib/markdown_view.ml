open! Core
open! Import
module Inline = Markdown.Inline
module Block = Markdown.Block

let cls name = [ Attr.class_ name ]

let copy_button =
  Node.button
    ~attrs:[ Attr.class_ "copy"; Attr.type_ "button"; Attr.title "Copy" ]
    [ Node.text "Copy" ]
;;

let code_block ~lang ~text ~closed =
  Node.div
    ~attrs:
      [ Attr.classes ([ "code"; "copyable" ] @ if closed then [] else [ "open" ])
      ]
    [ Node.div
        ~attrs:(cls "code-head")
        [ Node.span
            ~attrs:(cls "code-lang")
            [ Node.text (if String.is_empty lang then "text" else lang) ]
        ; copy_button
        ]
    ; Node.pre
        ~attrs:(cls "copy-text")
        [ Node.code
            ~attrs:
              (if String.is_empty lang then [] else cls ("language-" ^ lang))
            (Highlight_view.code (Highlight.Lang.of_name lang) text)
        ]
    ]
;;

let rec inline (i : Inline.t) =
  match i with
  | Text s -> Node.text s
  | Code s -> Node.code [ Node.text s ]
  | Strong l -> Node.strong (inlines l)
  | Emph l -> Node.em (inlines l)
  | Strike l -> Node.del (inlines l)
  | Break -> Node.br ()
  | Link { href; children } ->
    Node.a
      ~attrs:
        [ Attr.href href
        ; Attr.target "_blank"
        ; Attr.create "rel" "noopener noreferrer"
        ]
      (inlines children)

and inlines l = List.map l ~f:inline

let align_attr (align : Markdown.Align.t) =
  match align with
  | Default -> []
  | Left -> cls "left"
  | Center -> cls "center"
  | Right -> cls "right"
;;

(* Each block takes its direction from its text (Hebrew, Arabic...). *)
let auto = [ Attr.create "dir" "auto" ]

let rec block ?(tight = false) (b : Block.t) =
  match b with
  | Paragraph l when tight -> Node.fragment (inlines l)
  | Paragraph l -> Node.p ~attrs:auto (inlines l)
  | Heading (level, l) ->
    let h =
      match level with
      | 1 -> Node.h1
      | 2 -> Node.h2
      | 3 -> Node.h3
      | 4 -> Node.h4
      | 5 -> Node.h5
      | _ -> Node.h6
    in
    h ~attrs:auto (inlines l)
  | Code { lang; text; closed } -> code_block ~lang ~text ~closed
  | Quote blocks -> Node.blockquote ~attrs:auto (List.map blocks ~f:block)
  | Rule -> Node.hr ()
  | List { start; tight; items } ->
    let item ({ checked; blocks } : Block.item) =
      let content = List.map blocks ~f:(block ~tight) in
      match checked with
      | None -> Node.li ~attrs:auto content
      | Some checked ->
        Node.li
          ~attrs:(auto @ cls "task")
          (Node.input
             ~attrs:
               ([ Attr.type_ "checkbox"; Attr.disabled ]
                @ if checked then [ Attr.checked ] else [])
             ()
           :: content)
    in
    (match start with
     | None -> Node.ul (List.map items ~f:item)
     | Some start ->
       Node.ol
         ~attrs:
           (if start = 1
            then []
            else [ Attr.create "start" (Int.to_string start) ])
         (List.map items ~f:item))
  | Table { aligns; header; rows } ->
    let row ~head cells =
      Node.tr
        (List.map2_exn aligns cells ~f:(fun align c ->
           let attrs = align_attr align in
           if head
           then Node.th ~attrs (inlines c)
           else Node.td ~attrs (inlines c)))
    in
    Node.div
      ~attrs:(cls "table-wrap")
      [ Node.table
          [ Node.thead [ row ~head:true header ]
          ; Node.tbody (List.map rows ~f:(row ~head:false))
          ]
      ]
;;

let blocks ~streaming text =
  List.map (Markdown.parse ~partial:streaming text) ~f:block
;;

let cached table text ~f =
  if Hashtbl.length table > 2000 then Hashtbl.clear table;
  Hashtbl.find_or_add table text ~default:(fun () -> f text)
;;

(* The whole transcript is rendered on every event: finished texts are parsed
   once. *)
let cache = String.Table.create ()

(* A streaming reply's complete blocks, parsed once each (as lazy nodes that
   virtual_dom skips when they are reused): only its last part is parsed
   again at each delta. *)
let parts = String.Table.create ()

let part text =
  Node.lazy_
    (lazy (Node.div ~attrs:(cls "md-part") (blocks ~streaming:false text)))
;;

let render_streaming text =
  let rec split pos = function
    | [] -> [ String.drop_prefix text pos ]
    | start :: rest ->
      String.sub text ~pos ~len:(start - pos) :: split start rest
  in
  let pieces = split 0 (Markdown.block_starts text) in
  let settled = List.drop_last_exn pieces in
  let last = List.last_exn pieces in
  Node.div
    ~attrs:(cls "markdown")
    (List.map settled ~f:(cached parts ~f:part)
     @ [ Node.div ~attrs:(cls "md-part") (blocks ~streaming:true last) ])
;;

let render ?(streaming = false) text =
  if streaming
  then render_streaming text
  else
    cached cache text ~f:(fun text ->
      Node.div ~attrs:(cls "markdown") (blocks ~streaming:false text))
;;
