open! Core
open! Import

module Busy = struct
  type t =
    | Staging
    | Committing
    | Pushing
  [@@deriving sexp_of, equal]
end

module Stage = struct
  type t =
    | Clean
    | Unstaged
    | Partly
    | Staged
  [@@deriving sexp_of, equal]
end

type t =
  { dir : string option
  ; status : Git_status.t option
  ; error : string option
  ; commit : String.Set.t option
  ; message : string
  ; amend : bool
  ; busy : Busy.t option
  ; failure : string option
  ; notice : string option
  }
[@@deriving sexp_of, equal]

let empty =
  { dir = None
  ; status = None
  ; error = None
  ; commit = None
  ; message = ""
  ; amend = false
  ; busy = None
  ; failure = None
  ; notice = None
  }
;;

module Reply = struct
  type t =
    | Status
    | Staged
    | Staged_to_draft
    | Committed
    | Pushed
  [@@deriving sexp_of, equal]
end

module Out = struct
  type t =
    | Rpc of
        { method_ : Request.Method.t
        ; reply : Reply.t
        }
    | Draft_message
    | Changed
    | Create_pr of { dir : string option }
  [@@deriving sexp_of, equal]
end

module Action = struct
  type t =
    | Stage of string list
    | Unstage of string list
    | Compose of string list
    | Leave_out of string
    | Close_commit
    | Message of string
    | Toggle_amend
    | Commit
    | Push
    | Create_pr
    | Draft
  [@@deriving sexp_of, equal]
end

module Files = struct
  type t = (string * bool) list
end

let status_rpc t =
  Out.Rpc { method_ = Git_status { dir = t.dir }; reply = Status }
;;

let watch t ~dir =
  let t =
    if Option.equal String.equal t.dir dir && Option.is_some t.status
    then t
    else
      { empty with
        dir
      ; message =
          (if Option.equal String.equal t.dir dir then t.message else "")
      }
  in
  t, [ status_rpc t ]
;;

let file t path = Option.bind t.status ~f:(fun s -> Git_status.find s path)

let stage t path : Stage.t =
  match file t path with
  | None -> Clean
  | Some f ->
    (match Git_status.File.staged f, Git_status.File.unstaged f with
     | true, true -> Partly
     | true, false -> Staged
     | false, true -> Unstaged
     | false, false -> Clean)
;;

let approved_changes t ~(files : Files.t) =
  List.filter_map files ~f:(fun (path, approved) ->
    Option.some_if (approved && not (Stage.equal (stage t path) Clean)) path)
;;

let chosen t = Option.value t.commit ~default:String.Set.empty

let pending t =
  Set.to_list (chosen t)
  |> List.filter_map ~f:(fun path ->
    match file t path with
    | Some f when Git_status.File.unstaged f -> Some f.path
    | Some _ | None -> None)
  |> List.dedup_and_sort ~compare:String.compare
;;

let to_commit t =
  let staged =
    Option.value_map t.status ~default:[] ~f:(fun s ->
      List.filter_map s.files ~f:(fun f ->
        Option.some_if (Git_status.File.staged f) f.path))
  in
  let chosen =
    Set.to_list (chosen t)
    |> List.filter ~f:(fun path -> not (Stage.equal (stage t path) Clean))
  in
  List.dedup_and_sort (staged @ chosen) ~compare:String.compare
;;

let on_branch t = Option.exists t.status ~f:(fun s -> Option.is_some s.branch)

let rpc t method_ reply ~busy =
  ( { t with busy = Some busy; failure = None; notice = None }
  , [ Out.Rpc { method_; reply } ] )
;;

let stage_rpc t paths ~reply =
  rpc t (Git_stage { dir = t.dir; paths; tracked = false }) reply ~busy:Staging
;;

let unstage_rpc t paths =
  rpc t (Git_unstage { dir = t.dir; paths }) Staged ~busy:Staging
;;

(* The files git has, as it names them (a rename by its new path). *)
let known t paths ~f =
  List.filter_map paths ~f:(fun path ->
    Option.bind (file t path) ~f:(fun g -> Option.some_if (f g) g.path))
  |> List.dedup_and_sort ~compare:String.compare
;;

let refuse t text = { t with failure = Some text; notice = None }, []

let update t (action : Action.t) =
  match action with
  | Message message -> { t with message; failure = None }, []
  | Toggle_amend -> { t with amend = not t.amend; failure = None }, []
  | Create_pr -> t, [ Out.Create_pr { dir = t.dir } ]
  | Compose paths ->
    let paths =
      List.filter paths ~f:(fun path -> not (Stage.equal (stage t path) Clean))
    in
    ( { t with
        commit = Some (Set.union (chosen t) (String.Set.of_list paths))
      ; failure = None
      ; notice = None
      }
    , [] )
  | Close_commit -> { t with commit = None; failure = None }, []
  | _ when Option.is_some t.busy -> t, []
  | Leave_out path ->
    let t =
      { t with commit = Option.map t.commit ~f:(fun c -> Set.remove c path) }
    in
    (match file t path with
     | Some f when Git_status.File.staged f -> unstage_rpc t [ f.path ]
     | Some _ | None -> t, [])
  | Stage paths ->
    (match known t paths ~f:Git_status.File.unstaged with
     | [] -> refuse t "Nothing to stage: git has all of their changes."
     | paths -> stage_rpc t paths ~reply:Staged)
  | Unstage paths ->
    (match known t paths ~f:Git_status.File.staged with
     | [] ->
       refuse t "Nothing to unstage: git has none of their changes staged."
     | paths -> unstage_rpc t paths)
  | Commit ->
    if not (on_branch t)
    then refuse t "HEAD is detached: commit on a branch (git switch -c NAME)."
    else if (not t.amend) && List.is_empty (to_commit t)
    then
      refuse
        t
        "Nothing to commit: choose files with changes (right-click them, \
         Commit…)."
    else if (not t.amend) && String.is_empty (String.strip t.message)
    then refuse t "Write a commit message, or Generate one (Alt+G)."
    else
      rpc
        t
        (Git_commit
           { dir = t.dir
           ; message = t.message
           ; amend = t.amend
           ; paths = pending t
           })
        Committed
        ~busy:Committing
  | Push ->
    if not (on_branch t)
    then refuse t "HEAD is detached: push a branch (git switch -c NAME)."
    else rpc t (Git_push { dir = t.dir }) Pushed ~busy:Pushing
  | Draft ->
    (match pending t with
     | [] -> { t with failure = None }, [ Out.Draft_message ]
     | paths -> stage_rpc t paths ~reply:Staged_to_draft)
;;

let short sha = String.prefix sha 7

let reply t (reply : Reply.t) result =
  let decode f json ~ok =
    match f json with
    | Ok v -> ok v
    | Error e ->
      { t with busy = None; failure = Some (Error.to_string_hum e) }, []
  in
  match reply, result with
  | Status, Error e -> { t with error = Some e; status = None }, []
  | Status, Ok json ->
    decode Git_status.of_json json ~ok:(fun status ->
      { t with status = Some status; error = None }, [])
  | (Staged | Staged_to_draft | Committed | Pushed), Error e ->
    { t with busy = None; failure = Some e }, [ status_rpc t ]
  | Staged, Ok json ->
    decode Git_status.of_json json ~ok:(fun status ->
      { t with status = Some status; busy = None }, [])
  | Staged_to_draft, Ok json ->
    decode Git_status.of_json json ~ok:(fun status ->
      { t with status = Some status; busy = None }, [ Out.Draft_message ])
  | Committed, Ok json ->
    decode Git_status.Commit.of_json json ~ok:(fun c ->
      ( { t with
          status = Some c.status
        ; busy = None
        ; message = ""
        ; amend = false
        ; commit = None
        ; notice =
            Some
              (sprintf
                 "Committed %s %s%s"
                 (short c.sha)
                 c.subject
                 (Option.value_map c.status.branch ~default:"" ~f:(fun b ->
                    " on " ^ b)))
        }
      , [ Out.Changed ] ))
  | Pushed, Ok json ->
    decode Git_status.Push.of_json json ~ok:(fun p ->
      ( { t with
          status = Some p.status
        ; busy = None
        ; notice = Some (sprintf "Pushed %s to %s" p.branch p.remote)
        }
      , [] ))
;;
