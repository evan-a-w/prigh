open! Core
open! Import

module Outcome = struct
  type t =
    { downscaled : int
    ; unfit : string list
    }
  [@@deriving sexp_of]
end

module Key = struct
  type t =
    { digest : string
    ; limit : Image.Limit.t
    }
  [@@deriving sexp, equal, compare, hash]
end

module Cache = struct
  module Entry = struct
    type t =
      { fitted : Image.Fitted.t
      ; bytes : int
      ; mutable used : int
      }
  end

  type t =
    { entries : (Key.t, Entry.t) Hashtbl.t
    ; mutable bytes : int
    ; mutable clock : int
    ; mutable max_bytes : int
    }

  let t =
    { entries = Hashtbl.create (module Key)
    ; bytes = 0
    ; clock = 0
    ; max_bytes = 64 * 1024 * 1024
    }
  ;;

  let reset () =
    Hashtbl.clear t.entries;
    t.bytes <- 0
  ;;

  let find key =
    Option.map (Hashtbl.find t.entries key) ~f:(fun (e : Entry.t) ->
      t.clock <- t.clock + 1;
      e.used <- t.clock;
      e.fitted)
  ;;

  let remove key =
    Option.iter (Hashtbl.find_and_remove t.entries key) ~f:(fun e ->
      t.bytes <- t.bytes - e.bytes)
  ;;

  (* The least recently used go first; it is small, so a scan is cheap. *)
  let rec shrink () =
    if t.bytes > t.max_bytes
    then (
      match
        Hashtbl.fold t.entries ~init:None ~f:(fun ~key ~data:(e : Entry.t) acc ->
          match acc with
          | Some (_, (o : Entry.t)) when o.used <= e.used -> acc
          | _ -> Some (key, e))
      with
      | Some (key, _) ->
        remove key;
        shrink ()
      | None -> ())
  ;;

  let add key (fitted : Image.Fitted.t) =
    let bytes =
      String.length key.Key.digest
      +
      match fitted with
      | Downscaled image -> String.length image.data
      | Fits | Cannot _ -> 0
    in
    remove key;
    t.clock <- t.clock + 1;
    Hashtbl.set t.entries ~key ~data:{ fitted; bytes; used = t.clock };
    t.bytes <- t.bytes + bytes;
    shrink ()
  ;;
end

let reset () = Cache.reset ()

let set_max_bytes bytes =
  Cache.t.max_bytes <- bytes;
  Cache.shrink ()
;;

let cached () = Hashtbl.length Cache.t.entries

let count messages =
  List.sum
    (module Int)
    messages
    ~f:(function
      | Message.User u -> List.length u.images
      | Tool_result r -> List.length r.images
      | Assistant _ -> 0)
;;

let messages ~env ?cancel ?search_path limit messages =
  let downscaled = ref 0 in
  let unfit = ref [] in
  let fit_one (image : Image.t) =
    if not (Image.exceeds limit image)
    then image
    else (
      let key =
        { Key.digest = Digestif.SHA256.(to_hex (digest_string image.data))
        ; limit
        }
      in
      let fitted =
        match Cache.find key with
        | Some fitted -> fitted
        | None ->
          let fitted = Image.fit ~env ?cancel ?search_path limit image in
          (match fitted with
           | Fits | Downscaled _ -> Cache.add key fitted
           | Cannot _ -> ());
          fitted
      in
      match fitted with
      | Fits -> image
      | Downscaled smaller ->
        incr downscaled;
        smaller
      | Cannot why ->
        unfit := why :: !unfit;
        image)
  in
  let fit_all images = List.map images ~f:fit_one in
  let messages =
    List.map messages ~f:(fun (m : Message.t) : Message.t ->
      match m with
      | User u when not (List.is_empty u.images) ->
        User { u with images = fit_all u.images }
      | Tool_result r when not (List.is_empty r.images) ->
        Tool_result { r with images = fit_all r.images }
      | User _ | Tool_result _ | Assistant _ -> m)
  in
  messages, { Outcome.downscaled = !downscaled; unfit = List.rev !unfit }
;;
