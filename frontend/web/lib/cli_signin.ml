open! Core
open! Import

module Program = struct
  type t =
    | Tui
    | Tool_host
    | Web
    | Desktop
    | Other
  [@@deriving sexp_of, equal]

  let of_string = function
    | "tui" -> Tui
    | "tool-host" -> Tool_host
    | "web" -> Web
    | "desktop" -> Desktop
    | _ -> Other
  ;;

  let to_string = function
    | Tui -> "tui"
    | Tool_host -> "tool-host"
    | Web -> "web"
    | Desktop -> "desktop"
    | Other -> "other"
  ;;

  let name = function
    | Tui -> "prigh-tui"
    | Tool_host -> "prigh tool-host"
    | Web -> "prigh web"
    | Desktop -> "prigh desktop"
    | Other -> "A program"
  ;;
end

module Paste = struct
  type t =
    { key : string
    ; from : string option
    }
  [@@deriving sexp_of, equal]

  let is_base64url c =
    Char.is_alphanum c || Char.equal c '-' || Char.equal c '_'
  ;;

  let is_host_char c =
    Char.is_alphanum c || List.mem [ '.'; '-'; '_' ] c ~equal:Char.equal
  ;;

  let of_fields ~key ~from =
    Option.bind key ~f:(fun key ->
      Option.some_if
        (String.length key = 43 && String.for_all key ~f:is_base64url)
        { key
        ; from =
            Option.filter from ~f:(fun from ->
              (not (String.is_empty from))
              && String.length from <= 64
              && String.for_all from ~f:is_host_char)
        })
  ;;
end

module Request = struct
  type t =
    { port : int
    ; state : string
    ; program : Program.t
    ; paste : Paste.t option
    }
  [@@deriving sexp_of, equal]

  let is_hex c =
    Char.is_digit c || Char.between (Char.lowercase c) ~low:'a' ~high:'f'
  ;;

  let parse value ~program ~paste =
    match String.lsplit2 value ~on:'.' with
    | Some (port, state)
      when String.length port <= 5
           && String.for_all port ~f:Char.is_digit
           && (not (String.is_empty port))
           && String.length state >= 16
           && String.length state <= 128
           && String.for_all state ~f:is_hex ->
      let port = Int.of_string port in
      Option.some_if
        (port >= 1 && port <= 65535)
        { port; state; program = Program.of_string program; paste }
    | _ -> None
  ;;

  let of_hash hash =
    let fields =
      String.chop_prefix_if_exists hash ~prefix:"#"
      |> String.split ~on:'&'
      |> List.filter_map ~f:(String.lsplit2 ~on:'=')
    in
    let field name = List.Assoc.find fields ~equal:String.equal name in
    Option.bind (field "cli-signin") ~f:(fun value ->
      parse
        value
        ~program:(Option.value (field "program") ~default:"")
        ~paste:(Paste.of_fields ~key:(field "paste") ~from:(field "from")))
  ;;

  let to_string t =
    sprintf
      "cli-signin=%d.%s&program=%s%s"
      t.port
      t.state
      (Program.to_string t.program)
      (match t.paste with
       | None -> ""
       | Some { key; from } ->
         sprintf
           "&paste=%s%s"
           key
           (Option.value_map from ~default:"" ~f:(sprintf "&from=%s")))
  ;;

  let of_string = of_hash
end

let pending_key = "prigh.cli_signin"
let grant_path = "cli-signin/grant"

let grant_body ~user ~token =
  let string = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  Json.to_string (`Object [ "token", string token; "user", string user ])
;;

let cookie_of_grant text =
  match Json.parse text with
  | Ok json ->
    (match Json.field json "cookie" with
     | Some (`String cookie) when not (String.is_empty cookie) -> Some cookie
     | _ -> None)
  | Error _ -> None
;;

let callback_url (t : Request.t) = sprintf "http://127.0.0.1:%d/callback" t.port

module Answer = struct
  type t =
    | Allow of
        { cookie : string
        ; token : string option
        ; user : string option
        }
    | Cancel
end

let callback_fields (t : Request.t) (answer : Answer.t) =
  ("state", t.state)
  ::
  (match answer with
   | Cancel -> [ "cancel", "1" ]
   | Allow { cookie; token; user } ->
     [ "cookie", cookie
     ; "token", Option.value token ~default:""
     ; "user", Option.value user ~default:""
     ])
;;

let heading (t : Request.t) =
  match t.program with
  | Other -> "Allow a program to connect?"
  | program -> sprintf "Allow %s to connect?" (Program.name program)
;;

let where (t : Request.t) =
  match t.paste with
  | None -> "on this computer"
  | Some { from = Some from; _ } -> "on " ^ from
  | Some { from = None; _ } -> "on the computer that opened this link"
;;

let explanation (t : Request.t) ~host ~user =
  [ sprintf
      "%s %s wants to connect to %s%s."
      (Program.name t.program)
      (where t)
      host
      (match user with
       | Some user -> " as " ^ user
       | None -> " with this page's sign-in")
  ; "It gets this browser's sign-in to the site and your prigh sign-in. Allow \
     it only if you just started it."
  ]
;;

let paste_plaintext ~cookie ~token ~user =
  let string = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  Json.to_string
    (`Object
        [ "cookie", `String cookie; "token", string token; "user", string user ])
;;

let forward_hint (t : Request.t) =
  sprintf "ssh -L %d:127.0.0.1:%d" t.port t.port
;;

let seal_error (t : Request.t) ~secure ~name ~message =
  let instead =
    sprintf
      "or forward the port (%s when you connect) and choose Send to the \
       forwarded port."
      (forward_hint t)
  in
  if (not secure) || String.equal name "InsecureContextError"
  then
    sprintf
      "This page is not on https (or localhost), so the browser cannot make \
       the code: open the site over https, %s"
      instead
  else
    sprintf
      "This browser cannot make the code (%s: %s): use a recent Chrome, \
       Firefox or Safari, %s"
      name
      message
      instead
;;

module View = struct
  type t =
    | Ask of
        { working : bool
        ; error : string option
        }
    | Code of
        { code : string
        ; copied : [ `Not_yet | `Copied | `Failed ]
        }
    | No_code of string
    | Cancelled
  [@@deriving sexp_of]
end

let escape s =
  String.concat_map s ~f:(function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | c -> String.of_char c)
;;

let html (t : Request.t) ~host ~user (view : View.t) =
  let para ?(cls = "signin-sub") text =
    sprintf
      {|<p class="%s"%s>%s</p>|}
      cls
      (if String.is_prefix cls ~prefix:"signin-error"
       then {| role="alert"|}
       else "")
      (escape text)
  in
  let send_button =
    {|<button class="btn ghost" type="button" id="cli-signin-send">Send to the forwarded port</button>|}
  in
  let title, body =
    match view with
    | Ask { working; error } ->
      ( heading t
      , String.concat
          (List.map (explanation t ~host ~user) ~f:(fun line -> para line))
        ^ para ~cls:"signin-error" (Option.value error ~default:"")
        ^ sprintf
            {|<button class="btn primary" type="button" id="cli-signin-allow"%s>%s</button>
  <button class="btn ghost" type="button" id="cli-signin-cancel">Cancel</button>|}
            (if working then " disabled" else "")
            (if working then "Allowing…" else "Allow") )
    | Code { code; copied } ->
      ( sprintf
          "Paste this into the terminal %s"
          (match t.paste with
           | Some { from = Some from; _ } -> "on " ^ from
           | _ -> sprintf "where %s waits" (Program.name t.program))
      , para
          (sprintf
             "%s asked for it. The code works once, only for this sign-in."
             (Program.name t.program))
        ^ sprintf
            {|<textarea class="cli-signin-code" id="cli-signin-code" readonly rows="4" spellcheck="false" aria-label="The sign-in code">%s</textarea>|}
            (escape code)
        ^ para
            ~cls:
              (match copied with
               | `Failed -> "signin-error"
               | `Not_yet | `Copied -> "signin-error notice")
            (match copied with
             | `Not_yet -> ""
             | `Copied -> "Copied: paste it into the terminal and press Enter."
             | `Failed ->
               "Could not copy: select the code and copy it yourself.")
        ^ sprintf
            {|<button class="btn primary" type="button" id="cli-signin-copy">%s</button>
  %s|}
            (match copied with
             | `Copied -> "Copied"
             | `Not_yet | `Failed -> "Copy")
            send_button )
    | No_code why ->
      ( "This browser cannot make the code"
      , para ~cls:"signin-error" why ^ send_button )
    | Cancelled ->
      ( "Not allowed"
      , para
          (sprintf
             "Nothing was sent. Stop %s in its terminal (Ctrl-C) if it is \
              still waiting. You can close this tab."
             (Program.name t.program)) )
  in
  sprintf
    {|<div class="signin-page"><div class="signin cli-signin" role="dialog" aria-labelledby="cli-signin-title">
  <div class="brand"><span class="logo">prigh</span><span class="brand-sub">sign-in</span></div>
  <h1 id="cli-signin-title">%s</h1>
  %s
</div></div>|}
    (escape title)
    body
;;

let grant_error ~status ~body =
  let body = String.strip body in
  match status with
  | 401 ->
    ( (if String.is_empty body
       then "prigh did not accept this page's sign-in"
       else body)
      ^ ": sign in to prigh, then allow the program again."
    , `Sign_in )
  | 0 ->
    ( "The server could not be reached: check the connection and try again."
    , `Stop )
  | 404 ->
    ( "This prigh server cannot hand sign-ins to programs (it is older than \
       the program): update it, or exempt /ws and /terminal from the sign-in \
       proxy."
    , `Stop )
  | _ ->
    ( (if String.is_empty body
       then sprintf "The server answered %d." status
       else body)
    , `Stop )
;;
