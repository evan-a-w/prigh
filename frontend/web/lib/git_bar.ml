open! Core
open! Import

module Busy = struct
  type t =
    | Staging
    | Committing
    | Pushing
  [@@deriving sexp_of, equal]
end

module Tick = struct
  type t =
    | Clean
    | Checked
    | Partly
    | Unchecked
  [@@deriving sexp_of, equal]
end

type t =
  { dir : string option
  ; status : Git_status.t option
  ; error : string option
  ; excluded : String.Set.t
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
  ; excluded = String.Set.empty
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
    | Toggle of string
    | Stage_approved
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

let status_rpc t = Out.Rpc { method_ = Git_status { dir = t.dir }; reply = Status }

let watch t ~dir =
  let t =
    if Option.equal String.equal t.dir dir && Option.is_some t.status
    then t
    else { empty with dir; message = (if Option.equal String.equal t.dir dir then t.message else "") }
  in
  t, [ status_rpc t ]
;;

let file t path = Option.bind t.status ~f:(fun s -> Git_status.find s path)

let approved ~(files : Files.t) path =
  List.Assoc.find files ~equal:String.equal path |> Option.value ~default:false
;;

let tick t ~files path : Tick.t =
  match file t path with
  | None -> Clean
  | Some f ->
    let unstaged = Git_status.File.unstaged f in
    if Git_status.File.staged f
    then if unstaged then Partly else Checked
    else if approved ~files path && not (Set.mem t.excluded path)
    then Checked
    else Unchecked
;;

let pending t ~files =
  List.filter_map files ~f:(fun (path, _) ->
    match file t path, tick t ~files path with
    | Some f, Checked when not (Git_status.File.staged f) -> Some f.path
    | _ -> None)
  |> List.dedup_and_sort ~compare:String.compare
;;

let to_commit t ~files =
  let staged =
    Option.value_map t.status ~default:0 ~f:(fun s ->
      List.count s.files ~f:Git_status.File.staged)
  in
  staged + List.length (pending t ~files)
;;

let on_branch t =
  Option.exists t.status ~f:(fun s -> Option.is_some s.branch)
;;

let rpc t method_ reply ~busy =
  ( { t with busy = Some busy; failure = None; notice = None }
  , [ Out.Rpc { method_; reply } ] )
;;

let stage t paths ~reply =
  rpc t (Git_stage { dir = t.dir; paths; tracked = false }) reply ~busy:Staging
;;

let refuse t text = { t with failure = Some text; notice = None }, []

let update t ~files (action : Action.t) =
  match action with
  | Message message -> { t with message; failure = None }, []
  | Toggle_amend -> { t with amend = not t.amend; failure = None }, []
  | Create_pr -> t, [ Out.Create_pr { dir = t.dir } ]
  | _ when Option.is_some t.busy -> t, []
  | Toggle path ->
    (match file t path, tick t ~files path with
     | None, _ | _, Clean -> t, []
     | Some f, Checked when Git_status.File.staged f ->
       let t = { t with excluded = Set.add t.excluded path } in
       rpc
         t
         (Git_unstage { dir = t.dir; paths = [ f.path ] })
         Staged
         ~busy:Staging
     | Some _, Checked -> { t with excluded = Set.add t.excluded path }, []
     | Some f, (Partly | Unchecked) ->
       stage { t with excluded = Set.remove t.excluded path } [ f.path ] ~reply:Staged)
  | Stage_approved ->
    let paths =
      List.filter_map files ~f:(fun (path, approved) ->
        match file t path with
        | Some f when approved && Git_status.File.unstaged f -> Some f.path
        | Some _ | None -> None)
      |> List.dedup_and_sort ~compare:String.compare
    in
    (match paths with
     | [] -> refuse t "No approved files with changes to stage."
     | paths ->
       let excluded = List.fold paths ~init:t.excluded ~f:Set.remove in
       stage { t with excluded } paths ~reply:Staged)
  | Commit ->
    if not (on_branch t)
    then refuse t "HEAD is detached: commit on a branch (git switch -c NAME)."
    else if (not t.amend) && to_commit t ~files = 0
    then refuse t "Nothing to commit: tick the files to commit."
    else if (not t.amend) && String.is_empty (String.strip t.message)
    then refuse t "Write a commit message, or Generate one (Alt+G)."
    else
      rpc
        t
        (Git_commit
           { dir = t.dir
           ; message = t.message
           ; amend = t.amend
           ; paths = pending t ~files
           })
        Committed
        ~busy:Committing
  | Push ->
    if not (on_branch t)
    then refuse t "HEAD is detached: push a branch (git switch -c NAME)."
    else rpc t (Git_push { dir = t.dir }) Pushed ~busy:Pushing
  | Draft ->
    (match pending t ~files with
     | [] -> { t with failure = None }, [ Out.Draft_message ]
     | paths -> stage t paths ~reply:Staged_to_draft)
;;

let short sha = String.prefix sha 7

let reply t (reply : Reply.t) result =
  let decode f json ~ok =
    match f json with
    | Ok v -> ok v
    | Error e -> { t with busy = None; failure = Some (Error.to_string_hum e) }, []
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
        ; excluded = String.Set.empty
        ; notice =
            Some
              (sprintf
                 "Committed %s %s%s"
                 (short c.sha)
                 c.subject
                 (Option.value_map c.status.branch ~default:"" ~f:(fun b -> " on " ^ b)))
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
