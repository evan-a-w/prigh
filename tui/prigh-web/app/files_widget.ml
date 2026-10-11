open! Core
open Js_of_ocaml
module Files = Prigh_web.Files
module App = Prigh_web.App

let schedule = ref (fun (_ : App.Action.t) -> ())
let files : File.file Js.t Int.Table.t = Int.Table.create ()
let uploads : Js.Unsafe.any Int.Table.t = Int.Table.create ()
let next_id = ref 0

let choose dest list =
  let chosen =
    List.map list ~f:(fun (file : File.file Js.t) ->
      incr next_id;
      Hashtbl.set files ~key:!next_id ~data:file;
      { Files.Local_file.file = !next_id
      ; name = Js.to_string file##.name
      ; size = Float.to_int (Js.to_float (Js.Unsafe.get file "size"))
      })
  in
  if not (List.is_empty chosen)
  then !schedule (Files (Chosen { dest; files = chosen }))
;;

let files_of (l : File.fileList Js.t) =
  List.filter_map (List.init l##.length ~f:Fn.id) ~f:(fun i ->
    Js.Opt.to_option (l##item i))
;;

let in_panel (el : Dom_html.element Js.t) =
  match Dom_html.getElementById_opt "files-panel" with
  | None -> false
  | Some panel -> Js.to_bool ((Js.Unsafe.coerce panel)##contains el : bool Js.t)
;;

let forget id =
  Hashtbl.remove files id;
  Hashtbl.remove uploads id
;;

(* The reply's error ([{error}]) or the status line. *)
let failure xhr =
  let status : int = Js.Unsafe.get xhr "status" in
  let text = Js.to_string (Js.Unsafe.get xhr "responseText" : Js.js_string Js.t) in
  if status = 0
  then "the connection to the backend failed: check that it is up, then try again"
  else if String.is_empty (String.strip text)
  then sprintf "the backend answered %d" status
  else String.strip text
;;

let upload ~file ~url =
  match Hashtbl.find files file with
  | None ->
    !schedule
      (Files
         (Upload_done
            { file; result = Error "the file is gone from the page: choose it again" }))
  | Some blob ->
    let xhr = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "XMLHttpRequest") [||] in
    Hashtbl.set uploads ~key:file ~data:xhr;
    let finish result =
      forget file;
      !schedule (Files (Upload_done { file; result }))
    in
    Js.Unsafe.meth_call
      xhr
      "open"
      [| Js.Unsafe.inject (Js.string "PUT"); Js.Unsafe.inject (Js.string url) |]
    |> ignore;
    Js.Unsafe.set
      (Js.Unsafe.get xhr "upload")
      "onprogress"
      (Js.wrap_callback (fun ev ->
         let loaded = Float.to_int (Js.to_float (Js.Unsafe.get ev "loaded")) in
         !schedule (Files (Upload_progress { file; sent = loaded }))));
    Js.Unsafe.set
      xhr
      "onload"
      (Js.wrap_callback (fun _ ->
         let status : int = Js.Unsafe.get xhr "status" in
         if status = 200
         then (
           let text = Js.to_string (Js.Unsafe.get xhr "responseText" : Js.js_string Js.t) in
           finish
             (match Jsonaf.parse text with
              | Ok json -> Ok json
              | Error e -> Error (Error.to_string_hum e)))
         else finish (Error (failure xhr))));
    Js.Unsafe.set
      xhr
      "onerror"
      (Js.wrap_callback (fun _ -> finish (Error (failure xhr))));
    Js.Unsafe.set
      xhr
      "onabort"
      (Js.wrap_callback (fun _ -> forget file));
    Js.Unsafe.meth_call xhr "send" [| Js.Unsafe.inject blob |] |> ignore
;;

let download ~url ~name =
  let a = Dom_html.createA Dom_html.document in
  a##.href := Js.string url;
  a##setAttribute (Js.string "download") (Js.string name);
  a##.style##.display := Js.string "none";
  Dom.appendChild Dom_html.document##.body a;
  a##click;
  Dom.removeChild Dom_html.document##.body a
;;

let input_id : Files.Dest.t -> string = function
  | Here -> "files-upload-input"
  | Chat -> "chat-attach-input"
;;

let perform (page : Files.Page.t) =
  match page with
  | Pick dest ->
    Option.iter (Dom_html.getElementById_opt (input_id dest)) ~f:(fun el -> el##click)
  | Upload { file; url } -> upload ~file ~url
  | Abort_upload file ->
    (match Hashtbl.find uploads file with
     | Some xhr -> Js.Unsafe.meth_call xhr "abort" [||] |> ignore
     | None -> ());
    forget file
  | Forget file -> forget file
  | Download { url; name } -> download ~url ~name
  | Focus id ->
    Prigh_ui_web_app.Browser.after_render (fun () ->
      Option.iter (Dom_html.getElementById_opt id) ~f:(fun el -> el##focus))
;;

let drop_class = "dropping"

let set_drop target =
  let body = Dom_html.document##.body in
  List.iter [ "files"; "chat" ] ~f:(fun t ->
    let cls = Js.string (sprintf "%s-%s" drop_class t) in
    if Option.equal String.equal target (Some t)
    then body##.classList##add cls
    else body##.classList##remove cls)
;;

let drag_end () = set_drop None

let has_files (ev : Dom_html.dragEvent Js.t) =
  Js.Opt.case
    ev##.dataTransfer
    (fun () -> false)
    (fun data ->
       let types = Js.Unsafe.get data "types" in
       let n : int = Js.Unsafe.get types "length" in
       List.exists (List.init n ~f:Fn.id) ~f:(fun i ->
         String.equal (Js.to_string (Js.Unsafe.get types i : Js.js_string Js.t)) "Files"))
;;

let install ~schedule:s ~backend =
  schedule := s;
  s (Files (Set_base (Files.http_base backend)));
  let listen ?(capture = false) event handler =
    ignore
      (Dom_html.addEventListener
         Dom_html.document
         event
         (Dom.handler handler)
         (Js.bool capture)
       : Dom_html.event_listener_id)
  in
  (* The hidden pickers' choices. *)
  listen ~capture:true Dom_html.Event.change (fun ev ->
    Js.Opt.iter ev##.target (fun target ->
      match Dom_html.tagged target with
      | Input input ->
        let dest : Files.Dest.t option =
          match Js.to_string input##.id with
          | "files-upload-input" -> Some Here
          | "chat-attach-input" -> Some Chat
          | _ -> None
        in
        Option.iter dest ~f:(fun dest ->
          Js.Opt.iter input##.files (fun l -> choose dest (files_of l));
          input##.value := Js.string "")
      | _ -> ());
    Js._true);
  (* Where a drop would go, while files are dragged over the page. *)
  listen Dom_html.Event.dragover (fun (ev : Dom_html.dragEvent Js.t) ->
    if has_files ev
    then
      set_drop
        (Js.Opt.case
           ev##.target
           (fun () -> None)
           (fun target ->
              if Prigh_ui_web_app.Terminal_panel.contains target
              then None
              else if in_panel target
              then Some "files"
              else Some "chat"));
    Js._true);
  listen Dom_html.Event.dragleave (fun (ev : Dom_html.dragEvent Js.t) ->
    if Js.Opt.test (Js.Unsafe.get ev "relatedTarget") then () else drag_end ();
    Js._true)
;;
