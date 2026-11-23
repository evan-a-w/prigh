open! Core
open! Import
open Html
module Action = App.Action

let initial name = String.prefix (String.uppercase name) 1

module Who = struct
  type t =
    { name : string
    ; acting_as : string option
    ; host : string option
    }
end

let who (m : App.Model.t) : Who.t option =
  match m.hello, m.account with
  | Some ({ user = Some user; _ } as hello), account ->
    Some
      { name = user
      ; acting_as = Hello_reply.acting_as hello
      ; host = Option.map account ~f:Accounts.Account.host
      }
  | _, Some account ->
    Some
      { name = Accounts.Account.name account
      ; acting_as = None
      ; host = Some (Accounts.Account.host account)
      }
  | _, None -> None
;;

let avatar ?(size = `Default) name =
  Node.span
    ~attrs:
      [ Attr.class_
          ("avatar flex shrink-0 items-center justify-center rounded-full \
            bg-muted font-medium text-foreground "
           ^
           match size with
           | `Small -> "size-6 text-xs"
           | `Default -> "size-7 text-[13px]"
           | `Large -> "size-10 text-base")
      ; Attr.create "data-slot" "avatar"
      ]
    [ Node.text (initial name) ]
;;

let item_icon (id : string) : Icon.t =
  match id with
  | "add" -> User_plus
  | "act" -> Users
  | "back" -> Undo
  | "signout" -> Logout
  | "notify on" -> Bell
  | "notify off" -> Bell_off
  | "settings" -> Gear
  | "help" -> Help
  | _ -> User
;;

let menu (m : App.Model.t) (picker : Picker.t) ~inject =
  let close = inject Action.Close_dialog in
  let items = Picker.visible picker in
  let choose (item : Picker.Item.t) =
    Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
  in
  let row i (item : Picker.Item.t) =
    let selected = i = Picker.selected picker in
    if String.is_prefix item.id ~prefix:"switch "
    then
      (* Another account: its avatar, not an icon. *)
      Node.div
        ~attrs:
          [ classes [ "menu-item" ] [ "selected", selected ]
          ; Attr.create "data-slot" "dropdown-menu-item"
          ; Attr.role "menuitem"
          ; choose item
          ]
        [ avatar ~size:`Small item.label
        ; Node.span
            ~attrs:[ Attr.class_ "menu-text" ]
            [ span ~cls:"menu-label" item.label
            ; (if String.is_empty item.detail
               then Node.none
               else span ~cls:"menu-detail" item.detail)
            ]
        ]
    else
      Menu_view.item
        ~selected
        ~danger:(String.equal item.id "signout")
        ~icon:(item_icon item.id)
        ?hint:(if String.is_empty item.detail then None else Some item.detail)
        ~attrs:[ choose item ]
        item.label
  in
  let switches, actions =
    List.partition_tf
      (List.mapi items ~f:(fun i item -> i, item))
      ~f:(fun (_, item) -> String.is_prefix item.id ~prefix:"switch ")
  in
  Menu_view.backdrop
    ~on_close:close
    [ Menu_view.surface
        ~cls:
          "account-menu fixed bottom-[62px] left-2.5 max-h-[calc(100vh-80px)] \
           w-[300px] overflow-y-auto p-1 animate-in fade-in-0 zoom-in-95 \
           motion-reduce:animate-none"
        ~attrs:[ Attr.id "dialog" ]
        ~label:"Account"
        [ (match who m with
           | None -> Node.none
           | Some { name; acting_as = acting; host } ->
             div
               ~cls:
                 "account-current mb-1 flex items-center gap-2.5 border-b px-2 \
                  pt-2 pb-2.5"
               [ avatar ~size:`Large name
               ; div
                   ~cls:"account-who flex min-w-0 flex-1 flex-col leading-snug"
                   [ span ~cls:"account-name truncate text-sm font-medium" name
                   ; (match acting with
                      | Some ns ->
                        span
                          ~cls:"account-acting text-[13px] text-primary"
                          ("acting as " ^ ns)
                      | None -> Node.none)
                   ; (match host with
                      | Some host ->
                        span
                          ~cls:
                            "account-host truncate font-mono text-xs \
                             text-muted-foreground"
                          host
                      | None -> Node.none)
                   ]
               ; Loon.Badge.view
                   ~variant:Outline
                   ~class_:"account-badge text-[color:var(--sage)]"
                   [ Node.text "signed in" ]
               ])
        ; (match switches with
           | [] -> Node.none
           | switches ->
             Menu_view.section
               (Menu_view.heading "Switch to"
                :: List.map switches ~f:(fun (i, item) -> row i item)))
        ; Menu_view.section (List.map actions ~f:(fun (i, item) -> row i item))
        ; div
            ~cls:"menu-keys narrow:hidden"
            [ Node.text "↑↓ to move, Enter to choose, Esc to close" ]
        ]
    ]
;;
