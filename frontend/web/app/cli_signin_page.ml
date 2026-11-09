open! Core
open Js_of_ocaml
open Prigh_web

let storage () =
  Option.try_with (fun () ->
    Js.Optdef.to_option Dom_html.window##.sessionStorage)
  |> Option.join
;;

let pending () =
  Option.bind (storage ()) ~f:(fun storage ->
    Js.Opt.to_option (storage##getItem (Js.string Cli_signin.pending_key))
    |> Option.map ~f:Js.to_string)
  |> Option.bind ~f:Cli_signin.Request.of_string
;;

let set_pending request =
  Option.iter (storage ()) ~f:(fun storage ->
    match request with
    | Some request ->
      storage##setItem
        (Js.string Cli_signin.pending_key)
        (Js.string (Cli_signin.Request.to_string request))
    | None -> storage##removeItem (Js.string Cli_signin.pending_key))
;;

let take () =
  match Cli_signin.Request.of_hash (Browser.hash ()) with
  | Some request ->
    Browser.clear_hash ();
    set_pending None;
    Some request
  | None ->
    let request = pending () in
    set_pending None;
    request
;;

(* paste_seal.js *)
external paste_seal
  :  Js.js_string Js.t
  -> Js.js_string Js.t
  -> Js.js_string Js.t
  -> Js.Unsafe.any
  -> Js.Unsafe.any
  = "prigh_paste_seal"

(* The code for [plaintext], sealed to the program's key, or the error's
   name and message. *)
let seal (request : Cli_signin.Request.t) ~key ~plaintext ~f =
  let failed error =
    let field name =
      Option.try_with (fun () ->
        Js.to_string (Js.Unsafe.get error (Js.string name) : Js.js_string Js.t))
      |> Option.value ~default:""
    in
    f (Error (field "name", field "message"))
  in
  match
    paste_seal
      (Js.string key)
      (Js.string request.state)
      (Js.string plaintext)
      (Js.Unsafe.inject Js.undefined)
  with
  | exception Js_error.Exn e -> failed (Js.Unsafe.inject (Js_error.to_error e))
  | promise ->
    ignore
      (Js.Unsafe.meth_call
         promise
         "then"
         [| Js.Unsafe.inject
              (Js.wrap_callback (fun (code : Js.js_string Js.t) ->
                 f (Ok (Js.to_string code))))
          ; Js.Unsafe.inject (Js.wrap_callback failed)
         |]
       : Js.Unsafe.any)
;;

let on_click id f =
  Option.iter (Dom_html.getElementById_opt id) ~f:(fun el ->
    el##.onclick
    := Dom_html.handler (fun ev ->
         Dom.preventDefault ev;
         f ();
         Js._false))
;;

let on_escape f =
  Dom_html.document##.onkeydown
  := Dom_html.handler (fun ev ->
    match Js.Optdef.to_option ev##.key with
    | Some key when String.equal (Js.to_string key) "Escape" ->
      Dom_html.document##.onkeydown := Dom_html.no_handler;
      f ();
      Js._false
    | _ -> Js._true)
;;

let rec show request ~(login : Login.t) ~sign_in =
  render
    request
    ~login
    ~sign_in
    (Cli_signin.View.Ask { working = false; error = None })

and render
      ?cookie
      request
      ~(login : Login.t)
      ~sign_in
      (view : Cli_signin.View.t)
  =
  Browser.set_app_html
    (Cli_signin.html request ~host:(Browser.host ()) ~user:login.user view);
  let send answer =
    Browser.submit_form
      ~url:(Cli_signin.callback_url request)
      ~fields:(Cli_signin.callback_fields request answer)
  in
  match view with
  | Ask { working; error = _ } ->
    Option.iter
      (Dom_html.getElementById_coerce
         "cli-signin-allow"
         Dom_html.CoerceTo.button)
      ~f:(fun button -> if not working then button##focus);
    let cancel () =
      match request.paste with
      | None -> send Cancel
      | Some _ ->
        (* The listener is likely out of reach: say so here instead. *)
        render request ~login ~sign_in Cancelled
    in
    on_click "cli-signin-cancel" cancel;
    on_escape cancel;
    on_click "cli-signin-allow" (fun () ->
      if not working then allow request ~login ~sign_in)
  | Code { code; copied = _ } ->
    Dom_html.document##.onkeydown := Dom_html.no_handler;
    on_click "cli-signin-copy" (fun () ->
      Async_kernel.upon (Browser.copy_to_clipboard code) (fun copied ->
        render
          ?cookie
          request
          ~login
          ~sign_in
          (Code { code; copied = (if copied then `Copied else `Failed) })));
    on_click "cli-signin-send" (fun () ->
      granted_by_form request ~login ~cookie)
  | No_code _ ->
    Dom_html.document##.onkeydown := Dom_html.no_handler;
    on_click "cli-signin-send" (fun () ->
      granted_by_form request ~login ~cookie)
  | Cancelled -> Dom_html.document##.onkeydown := Dom_html.no_handler

(* The grant goes to the listener in a top-level form post: a navigation,
   which the browser lets a public page make to the loopback without a
   private network preflight or permission prompt. In paste mode it is the
   way for a forwarded port (ssh -L): a no-cors fetch would meet those
   checks, and its opaque answer could not tell the user whether it got
   through. *)
and granted_by_form request ~(login : Login.t) ~cookie =
  Option.iter cookie ~f:(fun cookie ->
    Browser.submit_form
      ~url:(Cli_signin.callback_url request)
      ~fields:
        (Cli_signin.callback_fields
           request
           (Allow { cookie; token = login.password; user = login.user })))

and allow request ~(login : Login.t) ~sign_in =
  render
    request
    ~login
    ~sign_in
    (Cli_signin.View.Ask { working = true; error = None });
  Browser.post_status
    ~content_type:"application/json"
    ~url:Cli_signin.grant_path
    ~body:(Cli_signin.grant_body ~user:login.user ~token:login.password)
    ~f:(fun ~status ~body ->
      match status, Cli_signin.cookie_of_grant body with
      | 200, Some cookie ->
        (match request.paste with
         | None -> granted_by_form request ~login ~cookie:(Some cookie)
         | Some { key; _ } ->
           let secure = Browser.secure_context () in
           let no_code ~name ~message =
             render
               ~cookie
               request
               ~login
               ~sign_in
               (No_code (Cli_signin.seal_error request ~secure ~name ~message))
           in
           if not secure
           then no_code ~name:"" ~message:""
           else
             seal
               request
               ~key
               ~plaintext:
                 (Cli_signin.paste_plaintext
                    ~cookie
                    ~token:login.password
                    ~user:login.user)
               ~f:(function
               | Ok code ->
                 render
                   ~cookie
                   request
                   ~login
                   ~sign_in
                   (Code { code; copied = `Not_yet })
               | Error (name, message) -> no_code ~name ~message))
      | _ ->
        let message, next =
          Cli_signin.grant_error
            ~status:(if status = 200 then 422 else status)
            ~body
        in
        (match next with
         | `Sign_in ->
           set_pending (Some request);
           sign_in ~error:message
         | `Stop ->
           render
             request
             ~login
             ~sign_in
             (Ask { working = false; error = Some message })))
;;
