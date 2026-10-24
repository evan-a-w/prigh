open! Core
open! Import

let max_files = 2000
let max_scopes = 200
let scope_arg = "$seen_scope"

let applies_to name =
  List.mem [ "read"; "write"; "edit"; "apply_patch" ] name ~equal:String.equal
;;

module Seen = struct
  type t =
    { digest : Md5.t
    ; at : float
    }
end

type t =
  { files : Seen.t String.Table.t
  ; mutable used : float
  }

let scopes : t String.Table.t = String.Table.create ()
let now () = Core_unix.gettimeofday ()

let evict_oldest table ~used ~max =
  if Hashtbl.length table > max
  then (
    let sorted =
      Hashtbl.to_alist table
      |> List.sort ~compare:(fun (_, a) (_, b) ->
        Float.compare (used a) (used b))
    in
    List.iter
      (List.take sorted (Hashtbl.length table - max))
      ~f:(fun (key, _) -> Hashtbl.remove table key))
;;

let with_scope arguments ~scope =
  match arguments with
  | `Object fields ->
    `Object
      ((scope_arg, `String scope)
       :: List.filter fields ~f:(fun (key, _) ->
         not (String.equal key scope_arg)))
  | other -> other
;;

let of_args arguments =
  match Json.member scope_arg arguments with
  | Some (`String scope) when not (String.is_empty scope) ->
    let t =
      Hashtbl.find_or_add scopes scope ~default:(fun () ->
        { files = String.Table.create (); used = now () })
    in
    t.used <- now ();
    evict_oldest scopes ~used:(fun (t : t) -> t.used) ~max:max_scopes;
    Some t
  | _ -> None
;;

let record t ~path ~contents =
  Hashtbl.set
    t.files
    ~key:path
    ~data:{ Seen.digest = Md5.digest_string contents; at = now () };
  evict_oldest t.files ~used:(fun (s : Seen.t) -> s.at) ~max:max_files
;;

let forget t ~path = Hashtbl.remove t.files path

let ago at =
  let seconds = Float.iround_down_exn (Float.max 0. (now () -. at)) in
  if seconds < 60
  then sprintf "%ds ago" seconds
  else if seconds < 3600
  then sprintf "%dm ago" (seconds / 60)
  else sprintf "%dh ago" (seconds / 3600)
;;

let check_overwrite t ~path ~current ~what ~retry =
  match t with
  | None -> Ok ()
  | Some t ->
    (match Hashtbl.find t.files path with
     | None ->
       Error
         (sprintf
            "refusing to %s: it already exists and you have not read it in \
             this session. Read %s first, then %s"
            what
            path
            retry)
     | Some seen when Md5.equal seen.digest (Md5.digest_string current) -> Ok ()
     | Some seen ->
       Error
         (sprintf
            "refusing to %s: it changed since you last read or wrote it (%s). \
             Read %s again, then %s"
            what
            (ago seen.at)
            path
            retry))
;;

let changed_note t ~path ~current =
  Option.bind t ~f:(fun t ->
    match Hashtbl.find t.files path with
    | Some seen when not (Md5.equal seen.digest (Md5.digest_string current)) ->
      Some
        (sprintf
           "note: %s changed since you last read or wrote it (%s); the edit \
            was matched against its current contents"
           path
           (ago seen.at))
    | Some _ | None -> None)
;;
