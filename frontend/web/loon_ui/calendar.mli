(** A calendar component that allows users to select a date or a range of
    dates. Port of shadcn/ui's [Calendar] (react-day-picker's DOM and
    behaviour: arrow keys move by a day or a week, Home and End to the
    week's ends, Page Up and Page Down by a month, with Shift by a year;
    Enter or Space selects).

    {[
      Calendar.component
        ~class_:"rounded-lg border"
        ~caption_layout:Dropdown
        ~default:(Single (Some (Calendar.today ())))
        graph
    ]} *)

open! Import

module Caption_layout : sig
  (** The month's caption: its name, or [<select>]s to jump to a month or a
      year. *)
  type t =
    | Label
    | Dropdown
    | Dropdown_months
    | Dropdown_years
  [@@deriving enumerate, sexp_of, compare, equal]

  (** react-day-picker's name: [label], [dropdown], ... *)
  val name : t -> string
end

module Range : sig
  (** [to_] is [None] while only the first day is picked. *)
  type t =
    { from : Date.t
    ; to_ : Date.t option
    }
  [@@deriving sexp_of, compare, equal]
end

module Selection : sig
  (** The selection, by mode (react-day-picker's [mode] and [selected]). *)
  type t =
    | Single of Date.t option
    | Multiple of Date.t list
    | Range of Range.t option
  [@@deriving sexp_of, compare, equal]

  (** react-day-picker's name of the mode: [single], [multiple], [range]. *)
  val mode : t -> string
end

(** The selection after [date] is clicked, as react-day-picker's: in single
    mode the clicked day (clicked again: none, unless [required]); in
    multiple mode the day added or removed (at most [max], at least [min]
    days); in range mode the range extended to it ([min], [max]: its length
    in nights; a range holding an [excluded] day starts again from
    [date], react-day-picker's [excludeDisabled]). *)
val select
  :  ?required:bool
  -> ?min:int
  -> ?max:int
  -> ?excluded:(Date.t -> bool)
  -> Selection.t
  -> Date.t
  -> Selection.t

(** The weeks shown for [month]'s month, from the week holding its first
    day to the one holding its last (six weeks with [fixed_weeks]). *)
val weeks : ?week_starts_on:Day_of_week.t -> ?fixed_weeks:bool -> Date.t -> Date.t list list

(** The week of the year (weeks start on Sunday; week 1 holds 1 January). *)
val week_number : Date.t -> int

(** The date a key moves the focus to from [date] (before skipping
    disabled days), or [None] for other keys. *)
val moved
  :  ?week_starts_on:Day_of_week.t
  -> shift:bool
  -> key:Js_of_ocaml.Dom_html.Keyboard_code.t
  -> Date.t
  -> Date.t option

(** A date formatted with date-fns's tokens, in English: [PPPP] ("Monday,
    June 9th, 2025"), [PPP] ("June 9th, 2025"), [PP] ("Jun 9, 2025"), [P]
    ("06/09/2025"), [LLLL]/[MMMM], [LLL]/[MMM], [LL]/[MM], [L]/[M], [dd],
    [d], [do], [yyyy]/[y], [yy], [EEEE], [EEE]; text in single quotes is
    copied, as are other characters. *)
val format : string -> Date.t -> string

(** The language of the captions, weekdays and days' labels, and the first
    day of the week (react-day-picker's [locale], from date-fns). *)
module Locale : sig
  type t

  (** English (United States), the default: weeks start on Sunday. *)
  val en_us : t

  (** Spanish: weeks start on Monday. *)
  val es : t

  (** Its BCP 47 tag: ["en-US"], ["es"]. *)
  val code : t -> string

  (** The month's caption: "June 2025", "junio 2025". *)
  val caption : t -> Date.t -> string
end

(** The browser's local date. *)
val today : unit -> Date.t

(** Controlled. [month] is the first month shown (any day of it), [focused]
    the day with the keyboard focus (react-day-picker keeps these itself:
    {!component} does). Days [disabled] says cannot be selected;
    [modifiers] add classes to the days they match (react-day-picker's
    [modifiers] and [modifiersClassNames]); [day_content] replaces a day's
    number (react-day-picker's [DayButton] children). [start_month] and
    [end_month] bound the navigation (with a year dropdown and no bounds:
    from 100 years ago to the end of this year). [id] (unique on the page,
    ["calendar"] by default) lets the keyboard move the focus.

    [locale] names the months and days ({!Locale.en_us} by default) and
    gives [week_starts_on]'s default; [format_weekday_name] (the weekdays'
    headers: "Su" by default) and [format_month_dropdown] (the month
    dropdown's options: "Jun") are react-day-picker's [formatters]. With
    [disable_navigation] the months shown stay put (the navigation buttons
    are disabled). With [exclude_disabled], a range cannot hold a disabled
    day (see {!select}). *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?locale:Locale.t
  -> ?format_weekday_name:(Day_of_week.t -> string)
  -> ?format_month_dropdown:(Month.t -> string)
  -> ?disable_navigation:bool
  -> ?exclude_disabled:bool
  -> ?caption_layout:Caption_layout.t
  -> ?button_variant:Button.Variant.t
  -> ?show_outside_days:bool
  -> ?show_week_number:bool
  -> ?fixed_weeks:bool
  -> ?number_of_months:int
  -> ?week_starts_on:Day_of_week.t
  -> ?start_month:Date.t
  -> ?end_month:Date.t
  -> ?disabled:(Date.t -> bool)
  -> ?modifiers:((Date.t -> bool) * string) list
  -> ?day_content:(Date.t -> outside:bool -> Node.t list)
  -> ?required:bool
  -> ?min:int
  -> ?max:int
  -> ?today:Date.t
  -> month:Date.t
  -> on_month_change:(Date.t -> unit Effect.t)
  -> focused:Date.t option
  -> on_focus:(Date.t option -> unit Effect.t)
  -> selected:Selection.t
  -> on_select:(Selection.t -> unit Effect.t)
  -> unit
  -> Node.t

type t =
  { view : Node.t
  ; selected : Selection.t
  ; set_selected : Selection.t -> unit Effect.t
  ; month : Date.t
  ; set_month : Date.t -> unit Effect.t
  }

(** Keeps the selection ([default] at first, which also sets the mode), the
    month shown ([default_month], or [today]'s, at first, within
    [start_month] and [end_month]) and the focused
    day. [today] defaults to the browser's local date. [on_select] runs
    after a day is clicked, with the new selection. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?locale:Locale.t
  -> ?format_weekday_name:(Day_of_week.t -> string)
  -> ?format_month_dropdown:(Month.t -> string)
  -> ?disable_navigation:bool
  -> ?exclude_disabled:bool
  -> ?caption_layout:Caption_layout.t
  -> ?button_variant:Button.Variant.t
  -> ?show_outside_days:bool
  -> ?show_week_number:bool
  -> ?fixed_weeks:bool
  -> ?number_of_months:int
  -> ?week_starts_on:Day_of_week.t
  -> ?start_month:Date.t
  -> ?end_month:Date.t
  -> ?disabled:(Date.t -> bool)
  -> ?modifiers:((Date.t -> bool) * string) list
  -> ?day_content:(Date.t -> outside:bool -> Node.t list)
  -> ?required:bool
  -> ?min:int
  -> ?max:int
  -> ?today:Date.t
  -> ?default_month:Date.t
  -> ?on_select:(Selection.t -> unit Effect.t) Bonsai.t
  -> default:Selection.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

module For_testing : sig
  (** Makes {!today} return this day (or the real one again, with [None]),
      for tests of what starts from today. *)
  val set_today : Date.t option -> unit
end
