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
            [ Node.text text ]
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

let rec block ?(tight = false) (b : Block.t) =
  match b with
  | Paragraph l when tight -> Node.fragment (inlines l)
  | Paragraph l -> Node.p (inlines l)
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
    h (inlines l)
  | Code { lang; text; closed } -> code_block ~lang ~text ~closed
  | Quote blocks -> Node.blockquote (List.map blocks ~f:block)
  | Rule -> Node.hr ()
  | List { start; tight; items } ->
    let item ({ checked; blocks } : Block.item) =
      let content = List.map blocks ~f:(block ~tight) in
      match checked with
      | None -> Node.li content
      | Some checked ->
        Node.li
          ~attrs:(cls "task")
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

let render text =
  Node.div ~attrs:(cls "markdown") (List.map (Markdown.parse text) ~f:block)
;;
