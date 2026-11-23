open! Import
open Js_of_ocaml

let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

module Make (Name : sig
    val name : string
  end) =
struct
  module Hook = Vdom.Attr.Hooks.Make (struct
      module State = Unit

      module Input = struct
        type t = Dom_html.element Js.t -> unit

        let sexp_of_t = sexp_of_opaque

        let combine f g el =
          f el;
          g el
        ;;
      end

      let init _ _ = ()

      let on_mount =
        `Schedule_immediately_after_this_dom_patch_completes
          (fun f () el -> f el)
      ;;

      let update ~old_input:_ ~new_input () el =
        ignore
          (Dom_html.window##requestAnimationFrame
             (Js.wrap_callback (fun (_ : Js.number_t) -> new_input el))
           : Dom_html.animation_frame_request_id)
      ;;

      let destroy _ () _ = ()
    end)

  let attr f = Vdom.Attr.create_hook Name.name (Hook.create f)
end

let hooks = String.Table.create ()

let attr ~name f =
  let make =
    Hashtbl.find_or_add hooks name ~default:(fun () ->
      let module M = Make (struct
          let name = "loon-after-render-" ^ name
        end)
      in
      M.attr)
  in
  make f
;;

let timers : (string, Js.Unsafe.any) Hashtbl.t = String.Table.create ()

let cancel_now key =
  match Hashtbl.find_and_remove timers key with
  | None -> ()
  | Some id -> Js.Unsafe.global##clearTimeout id
;;

let cancel key = Effect.of_thunk (fun () -> cancel_now key)

let schedule key ms eff =
  Effect.of_thunk (fun () ->
    cancel_now key;
    if in_browser ()
    then (
      let id =
        Js.Unsafe.global##setTimeout
          (Js.wrap_callback (fun () ->
             Hashtbl.remove timers key;
             Effect.Expert.handle_non_dom_event_exn eff))
          ms
      in
      Hashtbl.set timers ~key ~data:id))
;;

let by_id id =
  if in_browser ()
  then Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))
  else None
;;
