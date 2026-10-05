open! Core
open! Import
open Html
module Action = App.Action

let initial name = String.prefix (String.uppercase name) 1

let who (m : App.Model.t) =
  match m.hello, m.account with
  | Some ({ user = Some user; _ } as hello), account ->
    Some (user, Hello_reply.acting_as hello, Option.map account ~f:Accounts.Account.host)
  | _, Some account ->
    Some (Accounts.Account.name account, None, Some (Accounts.Account.host account))
  | _, None -> None
;;

let trigger (m : App.Model.t) ~inject =
  Option.map (who m) ~f:(fun (name, acting, _) ->
    Node.button
      ~attrs:
        [ Attr.class_ "account-button"
        ; Attr.type_ "button"
        ; Attr.title "Accounts: switch, add, act as, sign out"
        ; Attr.create "aria-haspopup" "dialog"
        ; Attr.on_click (fun _ -> inject Action.Open_accounts)
        ]
      [ span ~cls:"avatar" (initial name)
      ; div
          ~cls:"user"
          [ span ~cls:"user-name" name
          ; (match acting with
             | Some ns -> span ~cls:"user-acting" ("acting as " ^ ns)
             | None -> Node.none)
          ]
      ; icon ~cls:"chevron" Chevron
      ])
;;

let item_icon (id : string) : Icon.t =
  match id with
  | "add" -> User_plus
  | "act" -> Users
  | "back" -> Undo
  | "signout" -> Logout
  | _ -> User
;;

let menu (m : App.Model.t) (picker : Picker.t) ~inject =
  let close = inject Action.Close_dialog in
  let items = Picker.visible picker in
  let row i (item : Picker.Item.t) =
    let switch = String.is_prefix item.id ~prefix:"switch " in
    Node.div
      ~attrs:
        [ classes
            [ "menu-item" ]
            [ "selected", i = Picker.selected picker
            ; "danger", String.equal item.id "signout"
            ]
        ; Attr.role "menuitem"
        ; Attr.on_click (fun _ -> inject (Action.Picker_choose item.id))
        ]
      [ (if switch
         then span ~cls:"avatar small" (initial item.label)
         else icon (item_icon item.id))
      ; div
          ~cls:"menu-text"
          [ span ~cls:"menu-label" item.label
          ; (if String.is_empty item.detail
             then Node.none
             else span ~cls:"menu-detail" item.detail)
          ]
      ]
  in
  let switches, actions =
    List.partition_tf (List.mapi items ~f:(fun i item -> i, item)) ~f:(fun (_, item) ->
      String.is_prefix item.id ~prefix:"switch ")
  in
  Node.div
    ~attrs:
      [ Attr.class_ "menu-backdrop"
      ; Attr.on_click (fun ev ->
          if Js_of_ocaml.Js.Unsafe.equals ev##.target ev##.currentTarget
          then close
          else Effect.Ignore)
      ]
    [ Node.div
        ~attrs:
          [ Attr.class_ "account-menu"
          ; Attr.id "dialog"
          ; Attr.role "dialog"
          ; Attr.create "aria-label" "Account"
          ; Attr.tabindex (-1)
          ]
        [ (match who m with
           | None -> Node.none
           | Some (name, acting, host) ->
             div
               ~cls:"account-current"
               [ span ~cls:"avatar large" (initial name)
               ; div
                   ~cls:"account-who"
                   [ span ~cls:"account-name" name
                   ; (match acting with
                      | Some ns ->
                        span ~cls:"account-acting" ("acting as " ^ ns)
                      | None -> Node.none)
                   ; (match host with
                      | Some host -> span ~cls:"account-host" host
                      | None -> Node.none)
                   ]
               ; span ~cls:"account-badge" "signed in"
               ])
        ; (match switches with
           | [] -> Node.none
           | switches ->
             div
               ~cls:"menu-section"
               (span ~cls:"menu-heading" "Switch to"
                :: List.map switches ~f:(fun (i, item) -> row i item)))
        ; div
            ~cls:"menu-section"
            (List.map actions ~f:(fun (i, item) -> row i item))
        ; div ~cls:"menu-keys" [ Node.text "↑↓ move · Enter choose · Esc close" ]
        ]
    ]
;;
