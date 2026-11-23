open! Import
open Js_of_ocaml

module Caption_layout = struct
  type t =
    | Label
    | Dropdown
    | Dropdown_months
    | Dropdown_years
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Label -> "label"
    | Dropdown -> "dropdown"
    | Dropdown_months -> "dropdown-months"
    | Dropdown_years -> "dropdown-years"
  ;;
end

module Range = struct
  type t =
    { from : Date.t
    ; to_ : Date.t option
    }
  [@@deriving sexp_of, compare, equal]
end

module Selection = struct
  type t =
    | Single of Date.t option
    | Multiple of Date.t list
    | Range of Range.t option
  [@@deriving sexp_of, compare, equal]

  let mode = function
    | Single _ -> "single"
    | Multiple _ -> "multiple"
    | Range _ -> "range"
  ;;
end

(* react-day-picker's [addToRange]. *)
let add_to_range ~required ~min ~max date (range : Range.t option) : Range.t option =
  let range : Range.t option =
    match range with
    | None -> Some { from = date; to_ = (if min > 0 then None else Some date) }
    | Some { from; to_ = None } ->
      if Date.equal from date
      then
        if min = 0
        then Some { from; to_ = Some date }
        else if required
        then Some { from; to_ = None }
        else None
      else if Date.( < ) date from
      then Some { from = date; to_ = Some from }
      else Some { from; to_ = Some date }
    | Some { from; to_ = Some to_ } ->
      if Date.equal from date && Date.equal to_ date
      then if required then Some { from; to_ = Some to_ } else None
      else if Date.equal from date
      then Some { from; to_ = (if min > 0 then None else Some date) }
      else if Date.equal to_ date
      then Some { from = date; to_ = (if min > 0 then None else Some date) }
      else if Date.( < ) date from
      then Some { from = date; to_ = Some to_ }
      else Some { from; to_ = Some date }
  in
  match range with
  | Some { from; to_ = Some to_ } ->
    let nights = Date.diff to_ from in
    if (max > 0 && nights > max) || (min > 1 && nights < min)
    then Some { from = date; to_ = None }
    else range
  | _ -> range
;;

let rec select
  ?(required = false)
  ?(min = 0)
  ?(max = 0)
  ?excluded
  (selection : Selection.t)
  date
  : Selection.t
  =
  match excluded with
  | Some excluded ->
    (match select ~required ~min ~max selection date with
     | Range (Some { from; to_ = Some to_ })
       when List.exists
              (List.init (Date.diff to_ from + 1) ~f:(Date.add_days from))
              ~f:excluded -> Range (Some { from = date; to_ = None })
     | selection -> selection)
  | None ->
  match selection with
  | Single (Some d) when Date.equal d date -> if required then selection else Single None
  | Single _ -> Single (Some date)
  | Multiple dates ->
    if List.mem dates date ~equal:Date.equal
    then (
      let n = List.length dates in
      if (min > 0 && n <= min) || (required && n = 1)
      then selection
      else Multiple (List.filter dates ~f:(fun d -> not (Date.equal d date))))
    else if max > 0 && List.length dates >= max
    then selection
    else Multiple (dates @ [ date ])
  | Range range -> Range (add_to_range ~required ~min ~max date range)
;;

let first_of_month d = Date.create_exn ~y:(Date.year d) ~m:(Date.month d) ~d:1
let last_of_month d = Date.add_days (Date.add_months (first_of_month d) 1) (-1)

let start_of_week ~week_starts_on d =
  let back =
    (Day_of_week.to_int (Date.day_of_week d) - Day_of_week.to_int week_starts_on + 7) % 7
  in
  Date.add_days d (-back)
;;

let weeks ?(week_starts_on = Day_of_week.Sun) ?(fixed_weeks = false) month =
  let start = start_of_week ~week_starts_on (first_of_month month) in
  let n =
    if fixed_weeks
    then 6
    else (Date.diff (last_of_month month) start / 7) + 1
  in
  List.init n ~f:(fun w -> List.init 7 ~f:(fun d -> Date.add_days start ((7 * w) + d)))
;;

let week_number d =
  let sunday = start_of_week ~week_starts_on:Sun d in
  let saturday = Date.add_days sunday 6 in
  let week_one y = start_of_week ~week_starts_on:Sun (Date.create_exn ~y ~m:Jan ~d:1) in
  let w1 =
    let w = week_one (Date.year saturday) in
    if Date.( >= ) sunday w then w else week_one (Date.year saturday - 1)
  in
  (Date.diff sunday w1 / 7) + 1
;;

let moved ?(week_starts_on = Day_of_week.Sun) ~shift ~(key : Dom_html.Keyboard_code.t) d =
  match key with
  | ArrowLeft -> Some (Date.add_days d (-1))
  | ArrowRight -> Some (Date.add_days d 1)
  | ArrowUp -> Some (Date.add_days d (-7))
  | ArrowDown -> Some (Date.add_days d 7)
  | Home -> Some (start_of_week ~week_starts_on d)
  | End -> Some (Date.add_days (start_of_week ~week_starts_on d) 6)
  | PageUp -> Some (Date.add_months d (if shift then -12 else -1))
  | PageDown -> Some (Date.add_months d (if shift then 12 else 1))
  | _ -> None
;;

let month_name (m : Month.t) =
  match m with
  | Jan -> "January"
  | Feb -> "February"
  | Mar -> "March"
  | Apr -> "April"
  | May -> "May"
  | Jun -> "June"
  | Jul -> "July"
  | Aug -> "August"
  | Sep -> "September"
  | Oct -> "October"
  | Nov -> "November"
  | Dec -> "December"
;;

let day_name (d : Day_of_week.t) =
  match d with
  | Mon -> "Monday"
  | Tue -> "Tuesday"
  | Wed -> "Wednesday"
  | Thu -> "Thursday"
  | Fri -> "Friday"
  | Sat -> "Saturday"
  | Sun -> "Sunday"
;;

let ordinal n =
  let suffix =
    match n % 100 with
    | 11 | 12 | 13 -> "th"
    | _ ->
      (match n % 10 with
       | 1 -> "st"
       | 2 -> "nd"
       | 3 -> "rd"
       | _ -> "th")
  in
  Int.to_string n ^ suffix
;;

let format pattern date =
  let month = Date.month date in
  let m = Month.to_int month in
  let token = function
    | "PPPP" ->
      Some
        (sprintf
           "%s, %s %s, %d"
           (day_name (Date.day_of_week date))
           (month_name month)
           (ordinal (Date.day date))
           (Date.year date))
    | "PPP" ->
      Some (sprintf "%s %s, %d" (month_name month) (ordinal (Date.day date)) (Date.year date))
    | "PP" ->
      Some
        (sprintf
           "%s %d, %d"
           (String.prefix (month_name month) 3)
           (Date.day date)
           (Date.year date))
    | "P" -> Some (sprintf "%02d/%02d/%04d" m (Date.day date) (Date.year date))
    | "LLLL" | "MMMM" -> Some (month_name month)
    | "LLL" | "MMM" -> Some (String.prefix (month_name month) 3)
    | "LL" | "MM" -> Some (sprintf "%02d" m)
    | "L" | "M" -> Some (Int.to_string m)
    | "dd" -> Some (sprintf "%02d" (Date.day date))
    | "d" -> Some (Int.to_string (Date.day date))
    | "do" -> Some (ordinal (Date.day date))
    | "y" | "yyyy" -> Some (Int.to_string (Date.year date))
    | "yy" -> Some (sprintf "%02d" (Date.year date % 100))
    | "EEEE" -> Some (day_name (Date.day_of_week date))
    | "EEE" | "E" -> Some (String.prefix (day_name (Date.day_of_week date)) 3)
    | _ -> None
  in
  let buf = Buffer.create 32 in
  let n = String.length pattern in
  let rec go i =
    if i < n
    then (
      let c = pattern.[i] in
      if Char.equal c '\''
      then (
        let j =
          Option.value (String.index_from pattern (i + 1) '\'') ~default:n
        in
        Buffer.add_string buf (String.sub pattern ~pos:(i + 1) ~len:(j - i - 1));
        go (j + 1))
      else if Char.is_alpha c
      then (
        let j = ref i in
        while !j < n && Char.equal pattern.[!j] c do
          incr j
        done;
        let j = if Char.equal c 'd' && !j < n && Char.equal pattern.[!j] 'o' then !j + 1 else !j in
        let run = String.sub pattern ~pos:i ~len:(j - i) in
        Buffer.add_string buf (Option.value (token run) ~default:run);
        go j)
      else (
        Buffer.add_char buf c;
        go (i + 1)))
  in
  go 0;
  Buffer.contents buf
;;

module Locale = struct
  type t =
    { code : string
    ; month_names : string list
    ; day_names : string list
    ; short_day_names : string list
    ; week_starts_on : Day_of_week.t
    ; full_date : Date.t -> string
    }

  let days_from_sunday = List.init 7 ~f:Day_of_week.of_int_exn

  let en_us =
    { code = "en-US"
    ; month_names = List.map Month.all ~f:month_name
    ; day_names = List.map days_from_sunday ~f:day_name
    ; short_day_names =
        List.map days_from_sunday ~f:(fun d -> String.prefix (day_name d) 2)
    ; week_starts_on = Sun
    ; full_date = format "PPPP"
    }
  ;;

  let es_months =
    [ "enero"
    ; "febrero"
    ; "marzo"
    ; "abril"
    ; "mayo"
    ; "junio"
    ; "julio"
    ; "agosto"
    ; "septiembre"
    ; "octubre"
    ; "noviembre"
    ; "diciembre"
    ]
  ;;

  let es_days =
    [ "domingo"; "lunes"; "martes"; "miércoles"; "jueves"; "viernes"; "sábado" ]
  ;;

  (* date-fns's [es]: "PPPP" is "EEEE, d 'de' MMMM 'de' y". *)
  let es =
    { code = "es"
    ; month_names = es_months
    ; day_names = es_days
    ; short_day_names = [ "do"; "lu"; "ma"; "mi"; "ju"; "vi"; "sá" ]
    ; week_starts_on = Mon
    ; full_date =
        (fun d ->
          sprintf
            "%s, %d de %s de %d"
            (List.nth_exn es_days (Day_of_week.to_int (Date.day_of_week d)))
            (Date.day d)
            (List.nth_exn es_months (Month.to_int (Date.month d) - 1))
            (Date.year d))
    }
  ;;

  let code t = t.code
  let month_name t m = List.nth_exn t.month_names (Month.to_int m - 1)
  let day_name t d = List.nth_exn t.day_names (Day_of_week.to_int d)
  let short_day_name t d = List.nth_exn t.short_day_names (Day_of_week.to_int d)
  let caption t d = sprintf "%s %d" (month_name t (Date.month d)) (Date.year d)
end

let fixed_today = ref None

let today () =
  match !fixed_today with
  | Some today -> today
  | None ->
    let now = new%js Js.date_now in
    Date.create_exn
      ~y:now##getFullYear
      ~m:(Month.of_int_exn (now##getMonth + 1))
      ~d:now##getDate
;;

let visually_hidden =
  Attr.create
    "style"
    "border: 0; clip: rect(0 0 0 0); height: 1px; margin: -1px; overflow: hidden; \
     padding: 0; position: absolute; width: 1px; white-space: nowrap; word-wrap: normal"
;;

let chevron (orientation : [ `Left | `Right | `Down ]) =
  Icon.view
    ~class_:
      (match orientation with
       | `Left | `Right -> "size-4 rdp-chevron"
       | `Down -> "size-4 rdp-chevron")
    (match orientation with
     | `Left -> Chevron_left
     | `Right -> Chevron_right
     | `Down -> Chevron_down)
;;

let view
  ?(attrs = [])
  ?class_
  ?(id = "calendar")
  ?(locale = Locale.en_us)
  ?format_weekday_name
  ?(format_month_dropdown = fun m -> String.prefix (month_name m) 3)
  ?(disable_navigation = false)
  ?(exclude_disabled = false)
  ?(caption_layout = Caption_layout.Label)
  ?(button_variant = Button.Variant.Ghost)
  ?(show_outside_days = true)
  ?(show_week_number = false)
  ?(fixed_weeks = false)
  ?(number_of_months = 1)
  ?week_starts_on
  ?start_month
  ?end_month
  ?(disabled = fun _ -> false)
  ?(modifiers = [])
  ?day_content
  ?(required = false)
  ?(min = 0)
  ?(max = 0)
  ?today:today_opt
  ~month
  ~on_month_change
  ~focused
  ~on_focus
  ~selected
  ~on_select
  ()
  =
  let today = match today_opt with Some t -> t | None -> today () in
  let week_starts_on = Option.value week_starts_on ~default:locale.week_starts_on in
  let first = first_of_month month in
  let n = Int.max 1 number_of_months in
  let months = List.init n ~f:(fun i -> Date.add_months first i) in
  let last = List.last_exn months in
  let year_dropdown =
    match caption_layout with
    | Dropdown | Dropdown_years -> true
    | Label | Dropdown_months -> false
  in
  (* react-day-picker's navigation months: days outside them are hidden. *)
  let nav_start =
    match start_month with
    | Some s -> Some (first_of_month s)
    | None ->
      if year_dropdown
      then Some (Date.create_exn ~y:(Date.year today - 100) ~m:Jan ~d:1)
      else None
  in
  let nav_end =
    match end_month with
    | Some e -> Some (last_of_month e)
    | None ->
      if year_dropdown then Some (Date.create_exn ~y:(Date.year today) ~m:Dec ~d:31) else None
  in
  let start_bound = if disable_navigation then Some first else nav_start in
  let end_bound = if disable_navigation then Some (last_of_month last) else nav_end in
  let in_bounds d =
    Option.for_all start_bound ~f:(fun s -> Date.( >= ) d s)
    && Option.for_all end_bound ~f:(fun e -> Date.( <= ) d e)
  in
  let clamp d =
    let d = Option.value_map start_bound ~default:d ~f:(Date.max d) in
    Option.value_map end_bound ~default:d ~f:(Date.min d)
  in
  let visible d = List.mem months (first_of_month d) ~equal:Date.equal in
  let enabled d = (not (disabled d)) && in_bounds d in
  let is_selected d =
    match (selected : Selection.t) with
    | Single s -> Option.exists s ~f:(Date.equal d)
    | Multiple ds -> List.mem ds d ~equal:Date.equal
    | Range None -> false
    | Range (Some { from; to_ = None }) -> Date.equal d from
    | Range (Some { from; to_ = Some to_ }) -> Date.between d ~low:from ~high:to_
  in
  let range_flags d =
    match (selected : Selection.t) with
    | Range (Some { from; to_ }) ->
      ( Date.equal d from
      , Option.exists to_ ~f:(Date.equal d)
      , Option.exists to_ ~f:(fun to_ -> Date.( > ) d from && Date.( < ) d to_) )
    | _ -> false, false, false
  in
  let selected_dates =
    match (selected : Selection.t) with
    | Single s -> Option.to_list s
    | Multiple ds -> ds
    | Range None -> []
    | Range (Some { from; _ }) -> [ from ]
  in
  let focus_target =
    let candidates =
      Option.to_list focused @ selected_dates @ [ today ]
    in
    match List.find candidates ~f:(fun d -> visible d && enabled d) with
    | Some d -> Some d
    | None ->
      List.init (Date.diff (last_of_month last) first + 1) ~f:(Date.add_days first)
      |> List.find ~f:enabled
  in
  let show_month d =
    if visible d
    then Effect.Ignore
    else if Date.( < ) d first
    then on_month_change (first_of_month d)
    else on_month_change (Date.add_months (first_of_month d) (-(n - 1)))
  in
  let on_keydown d (ev : Dom_html.keyboardEvent Js.t) =
    let shift = Js.to_bool ev##.shiftKey in
    let key = Dom_util.key ev in
    match moved ~week_starts_on ~shift ~key d with
    | None -> Effect.Ignore
    | Some next ->
      let retry date =
        match key with
        | ArrowLeft | ArrowRight | ArrowUp | ArrowDown ->
          Option.value_exn (moved ~week_starts_on ~shift ~key date)
        | _ -> Date.add_days date (if Date.( < ) next d then -1 else 1)
      in
      let rec skip date attempts =
        let date = clamp date in
        if enabled date || attempts > 365
        then date
        else (
          let again = clamp (retry date) in
          if Date.equal again date then date else skip again (attempts + 1))
      in
      let target = skip next 0 in
      Effect.Many
        [ Dom_util.prevent_default ev
        ; on_focus (Some target)
        ; show_month target
        ; Dom_util.focus_selector
            (sprintf
               "[id=%S] td[data-day=%S]:not([data-outside]) button"
               id
               (Date.to_string target))
        ]
  in
  let day_cell display_month d =
    let outside = not (Date.equal (first_of_month d) display_month) in
    let hidden =
      (outside && not show_outside_days)
      || Option.exists nav_start ~f:(fun s -> Date.( < ) d s)
      || Option.exists nav_end ~f:(fun e -> Date.( > ) d e)
    in
    let is_disabled = not (enabled d) in
    let sel = is_selected d in
    let is_today = Date.equal d today in
    let is_focused = (not outside) && Option.exists focused ~f:(Date.equal d) in
    let range_start, range_end, range_middle = range_flags d in
    let flag b name = Option.some_if b (data name "true") in
    let classes =
      List.filter_opt
        [ Some
            ("group/day relative aspect-square h-full w-full rounded-(--cell-radius) p-0 \
              text-center select-none \
              [&:last-child[data-selected=true]_button]:rounded-r-(--cell-radius)"
             ^
             if show_week_number
             then
               " [&:nth-child(2)[data-selected=true]_button]:rounded-l-(--cell-radius) \
                rdp-day"
             else
               " [&:first-child[data-selected=true]_button]:rounded-l-(--cell-radius) \
                rdp-day")
        ; Option.some_if is_focused "rdp-focused"
        ; Option.some_if is_disabled "text-muted-foreground opacity-50 rdp-disabled"
        ; Option.some_if hidden "invisible rdp-hidden"
        ; Option.some_if
            outside
            "text-muted-foreground aria-selected:text-muted-foreground rdp-outside"
        ; Option.some_if
            is_today
            "rounded-(--cell-radius) bg-muted text-foreground \
             data-[selected=true]:rounded-none rdp-today"
        ; Option.some_if
            range_end
            "relative isolate z-0 rounded-r-(--cell-radius) bg-muted after:absolute \
             after:inset-y-0 after:left-0 after:w-4 after:bg-muted rdp-range_end"
        ; Option.some_if range_middle "rounded-none rdp-range_middle"
        ; Option.some_if
            range_start
            "relative isolate z-0 rounded-l-(--cell-radius) bg-muted after:absolute \
             after:inset-y-0 after:right-0 after:w-4 after:bg-muted rdp-range_start"
        ; Option.some_if sel "rdp-selected"
        ]
      @ List.filter_map modifiers ~f:(fun (matches, cls) -> Option.some_if (matches d) cls)
    in
    let label =
      (if is_today then "Today, " else "")
      ^ locale.full_date d
      ^ if sel then ", selected" else ""
    in
    let button =
      Button.view
        ~variant:Ghost
        ~size:Icon
        ~disabled:is_disabled
        ~class_:
          "relative isolate z-10 flex aspect-square size-auto w-full \
           min-w-(--cell-size) flex-col gap-1 border-0 leading-none font-normal \
           group-data-[focused=true]/day:relative group-data-[focused=true]/day:z-10 \
           group-data-[focused=true]/day:border-ring \
           group-data-[focused=true]/day:ring-[3px] \
           group-data-[focused=true]/day:ring-ring/50 \
           data-[range-end=true]:rounded-(--cell-radius) \
           data-[range-end=true]:rounded-r-(--cell-radius) \
           data-[range-end=true]:bg-primary \
           data-[range-end=true]:text-primary-foreground \
           data-[range-middle=true]:rounded-none data-[range-middle=true]:bg-muted \
           data-[range-middle=true]:text-foreground \
           data-[range-start=true]:rounded-(--cell-radius) \
           data-[range-start=true]:rounded-l-(--cell-radius) \
           data-[range-start=true]:bg-primary \
           data-[range-start=true]:text-primary-foreground \
           data-[selected-single=true]:bg-primary \
           data-[selected-single=true]:text-primary-foreground \
           dark:hover:text-foreground [&>span]:text-xs [&>span]:opacity-70 rdp-day \
           rdp-day_button"
        ~attrs:
          [ data
              "day"
              (sprintf "%d/%d/%d" (Month.to_int (Date.month d)) (Date.day d) (Date.year d))
          ; data
              "selected-single"
              (Bool.to_string (sel && not (range_start || range_end || range_middle)))
          ; data "range-start" (Bool.to_string range_start)
          ; data "range-end" (Bool.to_string range_end)
          ; data "range-middle" (Bool.to_string range_middle)
          ; Attr.tabindex
              (if (not outside) && Option.exists focus_target ~f:(Date.equal d)
               then 0
               else -1)
          ; aria "label" label
          ; Attr.on_click (fun _ ->
              Effect.Many
                [ on_select
                    (select
                       ~required
                       ~min
                       ~max
                       ?excluded:(Option.some_if exclude_disabled disabled)
                       selected
                       d)
                ; on_focus (Some d)
                ])
          ; Attr.on_focus (fun _ -> on_focus (Some d))
          ; Attr.on_blur (fun _ -> on_focus None)
          ; Attr.on_keydown (on_keydown d)
          ]
        (match day_content with
         | None -> [ Node.text (Int.to_string (Date.day d)) ]
         | Some f -> f d ~outside)
    in
    Node.td
      ~attrs:
        (Attr.classes classes
         :: Attr.role "gridcell"
         :: List.filter_opt
              [ Option.some_if sel (aria "selected" "true")
              ; Some (data "day" (Date.to_string d))
              ; Option.some_if
                  outside
                  (data "month" (format "yyyy-MM" display_month))
              ; flag sel "selected"
              ; flag is_disabled "disabled"
              ; flag hidden "hidden"
              ; flag outside "outside"
              ; flag is_focused "focused"
              ; flag is_today "today"
              ])
      (if hidden then [] else [ button ])
  in
  let dropdown ~label ~kind ~options ~value ~on_change ~caption =
    Node.span
      ~attrs:
        [ Attr.class_
            "has-focus:border-ring border-input has-focus:ring-ring/50 border \
             has-focus:ring-3 relative rounded-(--cell-radius) rdp-dropdown_root"
        ]
      [ Node.select
          ~attrs:
            [ Attr.classes
                [ "absolute inset-0 bg-popover opacity-0 rdp-dropdown"
                ; (match kind with
                   | `Months -> "rdp-months_dropdown"
                   | `Years -> "rdp-years_dropdown")
                ]
            ; aria "label" label
            ; Attr.on_change (fun _ v -> on_change (Int.of_string v))
            ]
          (List.map options ~f:(fun (v, text, disabled) ->
             Node.option
               ~attrs:
                 ([ Attr.value (Int.to_string v)
                  ; Attr.bool_property "selected" (v = value)
                  ]
                  @ if disabled then [ Attr.disabled ] else [])
               [ Node.text text ]))
      ; Node.span
          ~attrs:
            [ Attr.class_
                "h-6 pr-1 pl-1.5 font-medium select-none flex items-center gap-1 \
                 rounded-(--cell-radius) text-sm [&>svg]:size-3.5 \
                 [&>svg]:text-muted-foreground rdp-caption_label"
            ; aria "hidden" "true"
            ]
          [ Node.text caption; chevron `Down ]
      ]
  in
  let caption i display_month =
    let caption_text = Locale.caption locale display_month in
    let go_to m = on_month_change (clamp (Date.add_months m (-i))) in
    let months_dropdown () =
      let year = Date.year display_month in
      dropdown
        ~label:"Choose the Month"
        ~kind:`Months
        ~options:
          (List.init 12 ~f:(fun k ->
             let m = Date.create_exn ~y:year ~m:(Month.of_int_exn (k + 1)) ~d:1 in
             ( k
             , format_month_dropdown (Date.month m)
             , not
                 (Option.for_all start_bound ~f:(fun s ->
                    Date.( >= ) (last_of_month m) s)
                  && Option.for_all end_bound ~f:(fun e -> Date.( <= ) m e)) )))
        ~value:(Month.to_int (Date.month display_month) - 1)
        ~on_change:(fun k ->
          go_to (Date.create_exn ~y:year ~m:(Month.of_int_exn (k + 1)) ~d:1))
        ~caption:(format_month_dropdown (Date.month display_month))
    in
    let years_dropdown () =
      let low =
        Option.value_map start_bound ~default:(Date.year display_month) ~f:Date.year
      in
      let high =
        Option.value_map end_bound ~default:(Date.year display_month) ~f:Date.year
      in
      dropdown
        ~label:"Choose the Year"
        ~kind:`Years
        ~options:
          (List.init (high - low + 1) ~f:(fun k -> low + k, Int.to_string (low + k), false))
        ~value:(Date.year display_month)
        ~on_change:(fun y ->
          go_to (Date.create_exn ~y ~m:(Date.month display_month) ~d:1))
        ~caption:(Int.to_string (Date.year display_month))
    in
    Node.div
      ~attrs:
        [ Attr.class_
            "flex h-(--cell-size) w-full items-center justify-center px-(--cell-size) \
             rdp-month_caption"
        ]
      [ (match caption_layout with
         | Label ->
           Node.span
             ~attrs:
               [ Attr.class_ "font-medium select-none text-sm rdp-caption_label"
               ; Attr.role "status"
               ; aria "live" "polite"
               ]
             [ Node.text caption_text ]
         | Dropdown | Dropdown_months | Dropdown_years ->
           Node.div
             ~attrs:
               [ Attr.class_
                   "flex h-(--cell-size) w-full items-center justify-center gap-1.5 \
                    text-sm font-medium rdp-dropdowns"
               ]
             [ (match caption_layout with
                | Dropdown | Dropdown_months -> months_dropdown ()
                | _ -> Node.span [ Node.text (format "LLL" display_month) ])
             ; (match caption_layout with
                | Dropdown | Dropdown_years -> years_dropdown ()
                | _ -> Node.span [ Node.text (Int.to_string (Date.year display_month)) ])
             ; Node.span
                 ~attrs:[ Attr.role "status"; aria "live" "polite"; visually_hidden ]
                 [ Node.text caption_text ]
             ])
      ]
  in
  let weekdays =
    List.init 7 ~f:(fun i ->
      Day_of_week.of_int_exn ((Day_of_week.to_int week_starts_on + i) % 7))
  in
  let weekday_name =
    match format_weekday_name with
    | Some f -> f
    | None -> Locale.short_day_name locale
  in
  let month_view i display_month =
    Node.div
      ~attrs:[ Attr.class_ "flex w-full flex-col gap-4 rdp-month" ]
      [ caption i display_month
      ; Node.table
          ~attrs:
            [ Attr.role "grid"
            ; aria
                "multiselectable"
                (match selected with
                 | Single _ -> "false"
                 | Multiple _ | Range _ -> "true")
            ; aria "label" (Locale.caption locale display_month)
            ; Attr.class_ "w-full border-collapse rdp-month_grid"
            ]
          [ Node.thead
              ~attrs:[ aria "hidden" "true" ]
              [ Node.tr
                  ~attrs:[ Attr.class_ "flex rdp-weekdays" ]
                  ((if show_week_number
                    then
                      [ Node.th
                          ~attrs:
                            [ aria "label" "Week Number"
                            ; Attr.class_ "w-(--cell-size) select-none rdp-week_number_header"
                            ; Attr.create "scope" "col"
                            ]
                          []
                      ]
                    else [])
                   @ List.map weekdays ~f:(fun day ->
                     Node.th
                       ~attrs:
                         [ aria "label" (Locale.day_name locale day)
                         ; Attr.class_
                             "flex-1 rounded-(--cell-radius) text-[0.8rem] font-normal \
                              text-muted-foreground select-none rdp-weekday"
                         ; Attr.create "scope" "col"
                         ]
                       [ Node.text (weekday_name day) ]))
              ]
          ; Node.tbody
              ~attrs:[ Attr.class_ "rdp-weeks" ]
              (List.map (weeks ~week_starts_on ~fixed_weeks display_month) ~f:(fun week ->
                 let number = week_number (List.hd_exn week) in
                 Node.tr
                   ~attrs:[ Attr.class_ "mt-2 flex w-full rdp-week" ]
                   ((if show_week_number
                     then
                       [ Node.td
                           ~attrs:
                             [ Attr.class_
                                 "text-[0.8rem] text-muted-foreground select-none \
                                  rdp-week_number"
                             ; aria "label" (sprintf "Week %d" number)
                             ; Attr.role "rowheader"
                             ; Attr.create "scope" "row"
                             ]
                           [ Node.div
                               ~attrs:
                                 [ Attr.class_
                                     "flex size-(--cell-size) items-center \
                                      justify-center text-center"
                                 ]
                               [ Node.text (sprintf "%02d" number) ]
                           ]
                       ]
                     else [])
                    @ List.map week ~f:(day_cell display_month))))
          ]
      ]
  in
  let nav_button ~label ~enabled ~target ~class_ ~orientation =
    Node.button
      ~attrs:
        ([ Attr.type_ "button"
         ; Cn.attr
             [ Button.classes ~variant:button_variant ()
             ; "size-(--cell-size) p-0 select-none aria-disabled:opacity-50"
             ; class_
             ]
         ]
         @ (if enabled then [] else [ Attr.tabindex (-1); aria "disabled" "true" ])
         @ [ aria "label" label
           ; Attr.on_click (fun _ ->
               if enabled then on_month_change target else Effect.Ignore)
           ])
      [ chevron orientation ]
  in
  let previous = Date.add_months first (-1) in
  let next = Date.add_months first 1 in
  Node.div
    ~attrs:
      ([ data "slot" "calendar"
       ; Attr.id id
       ; Cn.attr
           ([ "w-fit rdp-root"
            ; "p-2 [--cell-radius:var(--radius-md)] [--cell-size:--spacing(7)] \
               group/calendar bg-background in-data-[slot=card-content]:bg-transparent \
               in-data-[slot=popover-content]:bg-transparent"
            ; "rtl:**:[.rdp-button\\_next>svg]:rotate-180"
            ; "rtl:**:[.rdp-button\\_previous>svg]:rotate-180"
            ]
            @ Option.to_list class_)
       ; data "mode" (Selection.mode selected)
       ]
       @ (if n > 1 then [ data "multiple-months" "true" ] else [])
       @ (if show_week_number then [ data "week-numbers" "true" ] else [])
       @ attrs)
    [ Node.div
        ~attrs:[ Attr.class_ "relative flex flex-col gap-4 md:flex-row rdp-months" ]
        (Node.create
           "nav"
           ~attrs:
             [ aria "label" "Navigation bar"
             ; Attr.class_
                 "absolute inset-x-0 top-0 flex w-full items-center justify-between \
                  gap-1 rdp-nav"
             ]
           [ nav_button
               ~label:"Go to the Previous Month"
               ~enabled:
                 (Option.for_all start_bound ~f:(fun s ->
                    Date.( >= ) previous (first_of_month s)))
               ~target:previous
               ~class_:"rdp-button_previous"
               ~orientation:`Left
           ; nav_button
               ~label:"Go to the Next Month"
               ~enabled:
                 (Option.for_all end_bound ~f:(fun e ->
                    Date.( <= ) (Date.add_months last 1) e))
               ~target:next
               ~class_:"rdp-button_next"
               ~orientation:`Right
           ]
         :: List.mapi months ~f:month_view)
    ]
;;

type t =
  { view : Node.t
  ; selected : Selection.t
  ; set_selected : Selection.t -> unit Effect.t
  ; month : Date.t
  ; set_month : Date.t -> unit Effect.t
  }

let component
  ?attrs
  ?class_
  ?locale
  ?format_weekday_name
  ?format_month_dropdown
  ?disable_navigation
  ?exclude_disabled
  ?caption_layout
  ?button_variant
  ?show_outside_days
  ?show_week_number
  ?fixed_weeks
  ?number_of_months
  ?week_starts_on
  ?start_month
  ?end_month
  ?disabled
  ?modifiers
  ?day_content
  ?required
  ?min
  ?max
  ?today:today_opt
  ?default_month
  ?on_select
  ~default
  (local_ graph)
  =
  let open Bonsai.Let_syntax in
  (* react-day-picker's [getInitialMonth]: kept within the bounds. *)
  let initial_month =
    let m =
      first_of_month
        (match default_month with
         | Some m -> m
         | None -> Option.value_or_thunk today_opt ~default:today)
    in
    let m =
      match end_month with
      | Some e ->
        let n = Option.value number_of_months ~default:1 in
        Date.min m (Date.add_months (first_of_month e) (-(n - 1)))
      | None -> m
    in
    match start_month with
    | Some s -> Date.max m (first_of_month s)
    | None -> m
  in
  let selected, set_selected = Bonsai.state default graph in
  let month, set_month = Bonsai.state initial_month graph in
  let focused, set_focused = Bonsai.state None graph in
  let id = Bonsai.path_id graph in
  let on_select = Option.value on_select ~default:(Bonsai.return (fun _ -> Effect.Ignore)) in
  let%arr selected and set_selected and month and set_month and focused and set_focused
  and id and on_select in
  { view =
      view
        ?attrs
        ?class_
        ~id:("loon" ^ id)
        ?locale
        ?format_weekday_name
        ?format_month_dropdown
        ?disable_navigation
        ?exclude_disabled
        ?caption_layout
        ?button_variant
        ?show_outside_days
        ?show_week_number
        ?fixed_weeks
        ?number_of_months
        ?week_starts_on
        ?start_month
        ?end_month
        ?disabled
        ?modifiers
        ?day_content
        ?required
        ?min
        ?max
        ?today:today_opt
        ~month
        ~on_month_change:set_month
        ~focused
        ~on_focus:set_focused
        ~selected
        ~on_select:(fun s -> Effect.Many [ set_selected s; on_select s ])
        ()
  ; selected
  ; set_selected
  ; month
  ; set_month
  }
;;

module For_testing = struct
  let set_today today = fixed_today := today
end
