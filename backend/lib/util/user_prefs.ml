open! Core
open! Import

type t =
  { path : string
  ; mutex : Eio.Mutex.t
  }

let path ~home = Filename.concat home ".prigh/prefs.json"
let create ~path = { path; mutex = Eio.Mutex.create () }

module Change = struct
  type t = string * Json.t option

  let to_json changes =
    `Object
      (List.map changes ~f:(fun (key, value) ->
         key, Option.value value ~default:`Null))
  ;;
end

let max_key = 200
let max_value = 256 * 1024
let max_file = 4 * 1024 * 1024

let read t =
  match In_channel.read_all t.path with
  | exception Sys_error _ -> Ok []
  | data when String.is_empty (String.strip data) -> Ok []
  | data ->
    (match Json.parse data with
     | Ok (`Object fields) -> Ok fields
     | Ok _ ->
       Or_error.errorf
         "%s must be a JSON object: fix it, or move it aside" t.path
     | Error e ->
       Or_error.errorf
         "%s is not valid JSON (%s): fix it, or move it aside"
         t.path
         (Error.to_string_hum e))
;;

let get ?keys t =
  Or_error.map (read t) ~f:(fun fields ->
    match keys with
    | None -> fields
    | Some keys ->
      List.filter fields ~f:(fun (key, _) ->
        List.mem keys key ~equal:String.equal))
;;

let write t fields =
  let data = Json.to_string_hum (`Object fields) ^ "\n" in
  if String.length data > max_file
  then
    Or_error.errorf
      "preferences are limited to %d MiB: remove some first"
      (max_file / 1024 / 1024)
  else
    Or_error.try_with (fun () ->
      Core_unix.mkdir_p (Filename.dirname t.path);
      let tmp = t.path ^ ".tmp" in
      Out_channel.write_all tmp ~data;
      Core_unix.rename ~src:tmp ~dst:t.path)
;;

(* Fibers of one process are serialised with the mutex, processes with the
   lock directory. *)
let update t ~f =
  Eio.Mutex.use_rw ~protect:false t.mutex
  @@ fun () ->
  Or_error.join
    (Or_error.try_with (fun () ->
       Core_unix.mkdir_p (Filename.dirname t.path);
       Lock_dir.with_lock
         ~file:t.path
         ~f:(fun () ->
           let open Or_error.Let_syntax in
           let%bind fields = read t in
           let%bind fields, result = f fields in
           let%map () = write t fields in
           result)
         ()))
;;

let check_key key =
  if String.is_empty key
  then Or_error.error_string "a preference's key must not be empty"
  else if String.length key > max_key
  then Or_error.errorf "a preference's key is limited to %d bytes" max_key
  else Ok ()
;;

let check_value key value =
  if String.length (Json.to_string value) > max_value
  then
    Or_error.errorf
      "the preference %S is limited to %d KiB"
      key
      (max_value / 1024)
  else Ok ()
;;

let check ((key, value) : Change.t) =
  Or_error.bind (check_key key) ~f:(fun () ->
    match value with
    | None -> Ok ()
    | Some value -> check_value key value)
;;

let assoc fields key = List.Assoc.find fields key ~equal:String.equal

(* In place, so that the file keeps its order. *)
let put fields key value =
  if Option.is_some (assoc fields key)
  then
    List.map fields ~f:(fun (k, v) ->
      if String.equal k key then k, value else k, v)
  else fields @ [ key, value ]
;;

let set t changes =
  let open Or_error.Let_syntax in
  let%bind () = Or_error.all_unit (List.map changes ~f:check) in
  update t ~f:(fun fields ->
    let fields, changed =
      List.fold
        changes
        ~init:(fields, [])
        ~f:(fun (fields, changed) ((key, value) as change) ->
          let same =
            Option.equal Json.exactly_equal (assoc fields key) value
          in
          let fields =
            match value with
            | None -> List.Assoc.remove fields key ~equal:String.equal
            | Some value -> put fields key value
          in
          ( fields
          , if same
            then changed
            else
              change
              :: List.filter changed ~f:(fun (k, _) ->
                not (String.equal k key)) ))
    in
    Ok (fields, List.rev changed))
;;

let push t ~key value ~max =
  let open Or_error.Let_syntax in
  let%bind () = check_key key in
  let%bind () = check_value key value in
  update t ~f:(fun fields ->
    let items =
      match assoc fields key with
      | Some (`Array items) -> items
      | Some _ | None -> []
    in
    let items =
      match items with
      | first :: _ when Json.exactly_equal first value -> items
      | items -> List.take (value :: items) (Int.max 1 max)
    in
    (* The oldest go first when the list grows too large; the newest stays
       ([check_value] took it). *)
    let size item = String.length (Json.to_string item) + 1 in
    let rec fit used = function
      | [] -> []
      | item :: rest ->
        let used = used + size item in
        if used > max_value then [] else item :: fit used rest
    in
    let list =
      match items with
      | [] -> `Array []
      | first :: rest -> `Array (first :: fit (size first + 1) rest)
    in
    Ok (put fields key list, list))
;;
