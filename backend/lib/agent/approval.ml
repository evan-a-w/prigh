open! Core
open! Import

module Rules = struct
  type t =
    { allow : Approval_rule.t list
    ; ask : Approval_rule.t list
    ; deny : Approval_rule.t list
    }
  [@@deriving sexp_of, equal]

  let empty = { allow = []; ask = []; deny = [] }

  let to_json t =
    let list rules =
      `Array (List.map rules ~f:(fun r -> `String (Approval_rule.to_string r)))
    in
    `Object [ "allow", list t.allow; "ask", list t.ask; "deny", list t.deny ]
  ;;

  let of_json json =
    match json with
    | `Object fields ->
      let open Or_error.Let_syntax in
      let list name =
        match List.Assoc.find fields ~equal:String.equal name with
        | None | Some `Null -> Ok ([], [])
        | Some (`Array items) ->
          Or_error.all
            (List.map items ~f:(function
               | `String s -> Ok (Approval_rule.of_string s)
               | _ ->
                 Or_error.errorf
                   "config.permissions.%s must be an array of strings"
                   name))
          |> Or_error.map ~f:(fun parsed ->
            ( List.filter_map parsed ~f:Result.ok
            , List.filter_map parsed ~f:(function
                | Ok _ -> None
                | Error e ->
                  Some
                    (sprintf
                       "permissions.%s: %s (left out)"
                       name
                       (Error.to_string_hum e))) ))
        | Some _ ->
          Or_error.errorf
            "config.permissions.%s must be an array of strings"
            name
      in
      let%bind allow, p1 = list "allow" in
      let%bind ask, p2 = list "ask" in
      let%bind deny, p3 = list "deny" in
      let unknown =
        List.filter_map fields ~f:(fun (name, _) ->
          if List.mem [ "allow"; "ask"; "deny" ] name ~equal:String.equal
          then None
          else
            Some
              (sprintf
                 "permissions.%s is not a list prigh knows (allow, ask, deny)"
                 name))
      in
      Ok ({ allow; ask; deny }, p1 @ p2 @ p3 @ unknown)
    | `Null -> Ok (empty, [])
    | _ ->
      Or_error.error_string
        "config.permissions must be an object with allow, ask and deny lists"
  ;;
end

module Decision = struct
  type t =
    | Allow
    | Ask of { rules : Approval_rule.t list }
    | Judge of { rules : Approval_rule.t list }
    | Deny of string
  [@@deriving sexp_of]
end

let is_edit name =
  List.mem [ "edit"; "write"; "apply_patch" ] name ~equal:String.equal
;;

(* Whether [path] is under [dir] and not in a [.git] directory there (its
   hooks and config run commands). *)
let in_project ~dir path =
  match
    String.chop_prefix
      path
      ~prefix:(String.rstrip dir ~drop:(Char.equal '/') ^ "/")
  with
  | None -> false
  | Some rest ->
    not
      (List.exists (String.split rest ~on:'/') ~f:(fun part ->
         String.equal part ".git" || String.equal part ".."))
;;

let plan_mode_refusal =
  "[plan mode] Plan mode is on: you are planning, not implementing, so files, \
   worktrees and pull requests cannot be changed. Only the user turns plan \
   mode off; do not try to get around this or ask them to. Keep investigating \
   with read-only tools, then present the plan in your reply and wait."
;;

let is_word_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'

let names_host text ~names =
  let text = String.lowercase text in
  List.exists names ~f:(fun name ->
    let name = String.lowercase (String.strip name) in
    (not (String.is_empty name))
    && List.exists
         (String.substr_index_all text ~may_overlap:false ~pattern:name)
         ~f:(fun i ->
           let j = i + String.length name in
           (i = 0 || not (is_word_char text.[i - 1]))
           && (j = String.length text || not (is_word_char text.[j]))))
;;

let decide
      ~(mode : Approval_mode.t)
      ~plan
      ?(host_permitted = true)
      ?(worktrees = [])
      ~(rules : Rules.t)
      ~session
      ~cwd
      ~home
      ~destructive
      ~changes_work
      (call : Approval_rule.Call.t)
  =
  let find rules f = List.find rules ~f:(fun r -> f r ~cwd ~home call) in
  let ask () = Decision.Ask { rules = Approval_rule.suggest call } in
  match find rules.deny Approval_rule.touches with
  | Some rule ->
    Decision.Deny
      (sprintf
         "[denied by the rule %s in permissions.deny; do not try to get around \
          it: ask the user]"
         (Approval_rule.to_string rule))
  | None ->
    if plan && changes_work
    then Deny plan_mode_refusal
    else if Option.is_some (find rules.ask Approval_rule.touches)
    then (* an allow rule could not cover it: ask rules come first *)
      Decision.Ask { rules = [] }
    else if not host_permitted
    then (
      match mode with
      | Smart -> Judge { rules = Approval_rule.suggest call }
      | Ask | Auto_edit | Auto -> ask ())
    else if Approval_rule.covers (rules.allow @ session) ~cwd ~home call
    then Allow
    else if not destructive
    then Allow
    else if plan
    then (
      match mode with
      | Smart -> Judge { rules = Approval_rule.suggest call }
      | Ask | Auto_edit | Auto -> ask ())
    else (
      match mode with
      | Ask -> ask ()
      | Auto_edit -> if is_edit call.tool then Allow else ask ()
      | Auto -> Allow
      | Smart ->
        let dirs = cwd :: worktrees in
        if
          is_edit call.tool
          && (not (List.is_empty call.paths))
          && List.for_all call.paths ~f:(fun path ->
            List.exists dirs ~f:(fun dir -> in_project ~dir path))
        then Allow
        else Judge { rules = Approval_rule.suggest call })
;;
