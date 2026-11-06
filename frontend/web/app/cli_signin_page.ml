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

let escape s =
  String.concat_map s ~f:(function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | c -> String.of_char c)
;;

let on_click id f =
  Option.iter (Dom_html.getElementById_opt id) ~f:(fun el ->
    el##.onclick
    := Dom_html.handler (fun ev ->
         Dom.preventDefault ev;
         f ();
         Js._false))
;;

let rec show request ~(login : Login.t) ~sign_in =
  render request ~login ~sign_in ~working:false ~error:None

and render request ~(login : Login.t) ~sign_in ~working ~error =
  Browser.set_app_html
    (sprintf
       {|<div class="signin-page"><div class="signin cli-signin" role="dialog" aria-labelledby="cli-signin-title">
  <div class="brand"><span class="logo">prigh</span><span class="brand-sub">sign-in</span></div>
  <h1 id="cli-signin-title">%s</h1>
  %s
  <p class="signin-error" role="alert">%s</p>
  <button class="btn primary" type="button" id="cli-signin-allow"%s>%s</button>
  <button class="btn ghost" type="button" id="cli-signin-cancel">Cancel</button>
</div></div>|}
       (escape (Cli_signin.heading request))
       (String.concat
          (List.map
             (Cli_signin.explanation
                request
                ~host:(Browser.host ())
                ~user:login.user)
             ~f:(fun line ->
               sprintf {|<p class="signin-sub">%s</p>|} (escape line))))
       (escape (Option.value error ~default:""))
       (if working then " disabled" else "")
       (if working then "Allowing…" else "Allow"));
  Option.iter
    (Dom_html.getElementById_coerce "cli-signin-allow" Dom_html.CoerceTo.button)
    ~f:(fun button -> if not working then button##focus);
  let cancel () =
    Browser.submit_form
      ~url:(Cli_signin.callback_url request)
      ~fields:(Cli_signin.callback_fields request Cancel)
  in
  on_click "cli-signin-cancel" cancel;
  Dom_html.document##.onkeydown
  := Dom_html.handler (fun ev ->
    match Js.Optdef.to_option ev##.key with
    | Some key when String.equal (Js.to_string key) "Escape" ->
      Dom_html.document##.onkeydown := Dom_html.no_handler;
      cancel ();
      Js._false
    | _ -> Js._true);
  on_click "cli-signin-allow" (fun () ->
    if not working
    then (
      render request ~login ~sign_in ~working:true ~error:None;
      Browser.post_status
        ~content_type:"application/json"
        ~url:Cli_signin.grant_path
        ~body:(Cli_signin.grant_body ~user:login.user ~token:login.password)
        ~f:(fun ~status ~body ->
          match status, Cli_signin.cookie_of_grant body with
          | 200, Some cookie ->
            Browser.submit_form
              ~url:(Cli_signin.callback_url request)
              ~fields:
                (Cli_signin.callback_fields
                   request
                   (Allow { cookie; token = login.password; user = login.user }))
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
                 ~working:false
                 ~error:(Some message)))))
;;
