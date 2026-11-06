open! Core
open! Import

module Image = struct
  type t =
    | Loading
    | Url of string
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { values : string String.Map.t
  ; sending : Json.t String.Map.t
  ; images : Image.t String.Map.t
  }
[@@deriving sexp_of]

let empty =
  { values = String.Map.empty
  ; sending = String.Map.empty
  ; images = String.Map.empty
  }
;;

let key ~call_id ~path name = sprintf "%s/%s/%s" call_id path name
let find t key = Map.find t.values key
let set t ~key value = { t with values = Map.set t.values ~key ~data:value }
let reply_key ~call_id ~component = call_id ^ "/" ^ component

let sending t ~call_id ~component =
  Map.find t.sending (reply_key ~call_id ~component)
;;

let send t (a : Ui_action.t) =
  { t with
    sending =
      Map.set
        t.sending
        ~key:(reply_key ~call_id:a.call_id ~component:a.component)
        ~data:a.values
  }
;;

let unsend t ~call_id ~component =
  { t with sending = Map.remove t.sending (reply_key ~call_id ~component) }
;;

let image t path = Map.find t.images path
let set_image t ~path image = { t with images = Map.set t.images ~key:path ~data:image }

let unasked t paths =
  List.dedup_and_sort paths ~compare:String.compare
  |> List.filter ~f:(fun p -> not (Map.mem t.images p))
;;
