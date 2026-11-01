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

let cache : (Key.t, Image.Fitted.t) Hashtbl.t = Hashtbl.create (module Key)
let reset () = Hashtbl.clear cache

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
        match Hashtbl.find cache key with
        | Some fitted -> fitted
        | None ->
          let fitted = Image.fit ~env ?cancel ?search_path limit image in
          (match fitted with
           | Fits | Downscaled _ -> Hashtbl.set cache ~key ~data:fitted
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
