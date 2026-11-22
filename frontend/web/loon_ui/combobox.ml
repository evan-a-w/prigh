(* Core has a Command too: name ours before Import opens Core. *)
module Cmdk = Command
open! Import

module Option_ = struct
  type t =
    { value : string
    ; label : string
    }
  [@@deriving sexp_of, equal]
end

let view
  ?(class_ = "w-[200px] justify-between")
  ?(content_class = "w-[200px] p-0")
  ?(id = "combobox")
  ?(placeholder = "Select an option...")
  ?(search_placeholder = "Search...")
  ?(empty = "No option found.")
  ~value
  ~on_value_change
  ~open_
  ~on_open_change
  ~command
  ~on_command_change
  (options : Option_.t list)
  =
  let on_open_change o =
    if o
    then on_open_change true
    else Effect.Many [ on_open_change false; on_command_change Cmdk.State.empty ]
  in
  Popover.view ~id ~open_ ~on_open_change (fun popover ->
    [ Popover.trigger
        popover
        (Button.view
           ~variant:Outline
           ~attrs:[ Attr.role "combobox" ]
           ~class_
           [ Node.text
               (match
                  List.find options ~f:(fun o ->
                    Option.equal String.equal value (Some o.value))
                with
                | Some o -> o.label
                | None -> placeholder)
           ; Icon.view ~class_:"opacity-50" Chevrons_up_down
           ])
    ; Popover.content
        popover
        ~class_:content_class
        [ Cmdk.view
            ~id:(id ^ "-command")
            ~state:command
            ~on_change:on_command_change
            [ Cmdk.input ~placeholder:search_placeholder ~class_:"h-9" ()
            ; Cmdk.list
                [ Cmdk.empty [ Node.text empty ]
                ; Cmdk.group
                    (List.map options ~f:(fun o ->
                       let chosen = Option.equal String.equal value (Some o.value) in
                       Cmdk.item
                         ~value:o.value
                         ~keywords:[ o.label ]
                         ~on_select:
                           (Effect.Many
                              [ on_value_change (if chosen then None else Some o.value)
                              ; on_open_change false
                              ])
                         [ Node.text o.label
                         ; Icon.view
                             ~class_:
                               (Cn.cn [ "ml-auto"; (if chosen then "opacity-100" else "opacity-0") ])
                             Check
                         ]))
                ]
            ]
        ]
    ])
;;

let component
  ?class_
  ?content_class
  ?placeholder
  ?search_placeholder
  ?empty
  ?default_value
  options
  (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let value, set_value = Bonsai.state_opt ?default_model:default_value graph in
  let open_, set_open = Bonsai.state false graph in
  let command, set_command = Bonsai.state Cmdk.State.empty graph in
  let id = Bonsai.path_id graph in
  let%arr value and set_value and open_ and set_open and command and set_command and id
  and options in
  ( value
  , view
      ?class_
      ?content_class
      ~id
      ?placeholder
      ?search_placeholder
      ?empty
      ~value
      ~on_value_change:set_value
      ~open_
      ~on_open_change:set_open
      ~command
      ~on_command_change:set_command
      options )
;;
