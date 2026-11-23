(** Tabler icons (https://tabler.io/icons, MIT licence), drawn as
    @tabler/icons-react 3.34.1 (the version shadcn/ui's blocks use) draws
    them: a 24x24 [<svg>], stroked with [currentColor] (filled with it for
    the [*_filled] icons), classes [tabler-icon tabler-icon-NAME]. Generated
    by tools/gen-icons.mjs: the icons are those in tools/tabler-icons.txt. *)

open! Import

type t =
  | Camera
  | Chart_bar
  | Chevron_down
  | Chevron_left
  | Chevron_right
  | Chevrons_left
  | Chevrons_right
  | Circle_check_filled
  | Circle_plus_filled
  | Credit_card
  | Dashboard
  | Database
  | Dots
  | Dots_vertical
  | File_ai
  | File_description
  | File_word
  | Folder
  | Grip_vertical
  | Help
  | Inner_shadow_top
  | Layout_columns
  | List_details
  | Loader
  | Logout
  | Mail
  | Notification
  | Plus
  | Report
  | Search
  | Settings
  | Share_3
  | Trash
  | Trending_down
  | Trending_up
  | User_circle
  | Users
[@@deriving enumerate, sexp_of, compare, equal]

(** Its file name in the package: [camera]. *)
val name : t -> string

val of_name : string -> t option

(** The icon. [class_] goes after [tabler-icon tabler-icon-NAME];
    [stroke_width] (["2"]) is for the outline icons. *)
val view : ?attrs:Attr.t list -> ?class_:string -> ?stroke_width:string -> t -> Node.t
