open! Core
open! Import

module Page = struct
  type t =
    { state : Ui_state.t
    ; inject : Ui_event.t -> unit Effect.t
    ; sent : Ui_action.Sent.t list
    }

  let none =
    { state = Ui_state.empty; inject = (fun _ -> Effect.Ignore); sent = [] }
  ;;
end

type t =
  { call_id : string
  ; state : Ui_state.t
  ; sent : Ui_action.Sent.t list
  ; inject : Ui_event.t -> unit Effect.t
  ; fold : Fold.t
  }

let create (page : Page.t) ~call_id ~fold =
  { call_id
  ; state = page.state
  ; sent =
      List.filter page.sent ~f:(fun (s : Ui_action.Sent.t) ->
        String.equal s.call_id call_id)
  ; inject = page.inject
  ; fold
  }
;;

let get t ~path name =
  Ui_state.find t.state (Ui_state.key ~call_id:t.call_id ~path name)
;;

let set t ~path name value =
  t.inject (Set { key = Ui_state.key ~call_id:t.call_id ~path name; value })
;;

let is_ui_action (u : Message.User.t) =
  Option.exists u.origin ~f:(fun (o : Origin.t) ->
    Option.exists o.source ~f:(fun s -> Origin.Kind.equal s.kind Ui_action)
    || List.exists o.notes ~f:(fun n -> Origin.Kind.equal n.kind Ui_action))
;;

let sent_of_messages users =
  List.filter_map users ~f:(fun (u : Message.User.t) ->
    if is_ui_action u
    then (
      let notes =
        Option.value_map u.origin ~default:[] ~f:(fun (o : Origin.t) ->
          List.filter_map o.notes ~f:(fun n ->
            Option.some_if (Origin.Kind.equal n.kind Ui_action) n.text))
      in
      List.find_map (u.text :: notes) ~f:Ui_action.Sent.of_text)
    else None)
;;

let sent_values t ~component =
  match
    List.find t.sent ~f:(fun (s : Ui_action.Sent.t) ->
      String.equal s.call_id t.call_id && String.equal s.component component)
  with
  | Some s -> Some (Option.value s.values ~default:`Null)
  | None -> Ui_state.sending t.state ~call_id:t.call_id ~component
;;

let md ?(cls = "") text =
  Node.div
    ~attrs:[ Attr.class_ (String.strip ("ui-md " ^ cls)) ]
    [ Markdown_view.render text ]
;;
