open! Import

module Size = struct
  type t =
    | Default
    | Sm
    | Lg
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Sm -> "sm"
    | Lg -> "lg"
  ;;
end

module Status = struct
  type t =
    | Loading
    | Loaded
    | Error
  [@@deriving enumerate, sexp_of, compare, equal]
end

type t =
  { status : Status.t
  ; on_status_change : Status.t -> unit Effect.t
  }

let view ?(attrs = []) ?class_ ?(size = Size.Default) ~status ~on_status_change children =
  Node.span
    ~attrs:
      ([ data "slot" "avatar"
       ; data "size" (Size.name size)
       ; Cn.with_class
           ?class_
           [ "size-8 rounded-full after:rounded-full data-[size=lg]:size-10 \
              data-[size=sm]:size-6 group/avatar relative flex shrink-0 select-none \
              after:absolute after:inset-0 after:border after:border-border \
              after:mix-blend-darken dark:after:mix-blend-lighten"
           ]
       ]
       @ attrs)
    (children { status; on_status_change })
;;

let component ?attrs ?class_ ?size children (local_ graph) =
  let open Bonsai.Let_syntax in
  let status, set_status = Bonsai.state Status.Loading graph in
  let%arr status and set_status and children in
  view ?attrs ?class_ ?size ~status ~on_status_change:set_status children
;;

let image ?(attrs = []) ?class_ ?alt t ~src () =
  match t.status with
  | Error -> Node.none
  | Loading | Loaded ->
    let report status _ =
      if Status.equal t.status status then Effect.Ignore else t.on_status_change status
    in
    Node.img
      ~attrs:
        ([ data "slot" "avatar-image"
         ; Cn.with_class ?class_ [ "rounded-full aspect-square size-full object-cover" ]
         ; Attr.src src
         ; Attr.on_load (report Loaded)
         ; Attr.on_error (report Error)
         ]
         @ Option.value_map alt ~default:[] ~f:(fun alt -> [ Attr.alt alt ])
         @ (match t.status with
            | Loading -> [ Attr.hidden ]
            | Loaded | Error -> [])
         @ attrs)
      ()
;;

let fallback ?(attrs = []) ?class_ t children =
  match t.status with
  | Loaded -> Node.none
  | Loading | Error ->
    Node.span
      ~attrs:
        ([ data "slot" "avatar-fallback"
         ; Cn.with_class
             ?class_
             [ "bg-muted text-muted-foreground rounded-full flex size-full items-center \
                justify-center text-sm group-data-[size=sm]/avatar:text-xs"
             ]
         ]
         @ attrs)
      children
;;

let badge ?(attrs = []) ?class_ children =
  Node.span
    ~attrs:
      ([ data "slot" "avatar-badge"
       ; Cn.with_class
           ?class_
           [ "text-primary-foreground ring-background absolute right-0 bottom-0 z-10 \
              inline-flex items-center justify-center rounded-full bg-blend-color \
              ring-2 select-none"
           ; "group-data-[size=sm]/avatar:size-2 group-data-[size=sm]/avatar:[&>svg]:hidden"
           ; "group-data-[size=default]/avatar:size-2.5 \
              group-data-[size=default]/avatar:[&>svg]:size-2"
           ; "group-data-[size=lg]/avatar:size-3 group-data-[size=lg]/avatar:[&>svg]:size-2"
           ]
       ]
       @ attrs)
    children
;;

let group ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "avatar-group"
       ; Cn.with_class
           ?class_
           [ "group/avatar-group flex -space-x-2 *:data-[slot=avatar]:ring-2 \
              *:data-[slot=avatar]:ring-background"
           ]
       ]
       @ attrs)
    children
;;

let group_count ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "avatar-group-count"
       ; Cn.with_class
           ?class_
           [ "bg-muted text-muted-foreground size-8 rounded-full text-sm \
              group-has-data-[size=lg]/avatar-group:size-10 \
              group-has-data-[size=sm]/avatar-group:size-6 [&>svg]:size-4 \
              group-has-data-[size=lg]/avatar-group:[&>svg]:size-5 \
              group-has-data-[size=sm]/avatar-group:[&>svg]:size-3 relative flex \
              shrink-0 items-center justify-center ring-2 ring-background"
           ]
       ]
       @ attrs)
    children
;;
