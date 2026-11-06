open! Core

(** The arguments of a [render_ui] call: the components the model shows
    (prigh-web renders them, the TUI a line each). The parse is tolerant: an
    unknown component type is [Unknown], a malformed field falls back to its
    default (an empty list, no title, ...), so a call from a newer backend
    or a model that strays still shows what it can. Strings documented as
    markdown take inline markdown. *)

module Tone : sig
  type t =
    | Neutral
    | Info
    | Success
    | Warning
    | Danger
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

(** A number or a string, as the model gave it. *)
module Scalar : sig
  type t =
    | Number of float
    | Text of string
  [@@deriving sexp_of, equal]
end

module Cell : sig
  type t =
    | Number of float
    | Text of string
    | Bool of bool
    | Null
  [@@deriving sexp_of, equal]
end

module Table : sig
  module Align : sig
    type t =
      | Left
      | Right
      | Center
    [@@deriving sexp_of, equal]
  end

  module Format : sig
    type t =
      | Text
      | Number
      | Percent
      | Currency
      | Date
      | Md
    [@@deriving sexp_of, equal]
  end

  module Column : sig
    type t =
      { key : string
      ; label : string (** the key when not given *)
      ; align : Align.t (** numbers right, the rest left, unless given *)
      ; format : Format.t
      ; currency : string (** ISO code, default [USD] *)
      ; decimals : int option
      }
    [@@deriving sexp_of]
  end

  type t =
    { title : string option
    ; columns : Column.t list
    ; rows : (string * Cell.t) list list
    ; sortable : bool
    ; caption : string option
    }
  [@@deriving sexp_of]

  val cell : (string * Cell.t) list -> string -> Cell.t
end

module Chart : sig
  module Kind : sig
    type t =
      | Line
      | Area
      | Bar
      | Stacked_bar
      | Horizontal_bar
      | Pie
      | Donut
      | Scatter
    [@@deriving sexp_of, equal, enumerate]

    val to_string : t -> string
  end

  module Height : sig
    type t =
      | Small
      | Medium
      | Large
    [@@deriving sexp_of, equal]
  end

  module Series : sig
    type t =
      { name : string
      ; values : float option list (** one per label; [None]: a gap *)
      }
    [@@deriving sexp_of]
  end

  module Points : sig
    type t =
      { name : string
      ; points : (float * float) list
      }
    [@@deriving sexp_of]
  end

  module Slice : sig
    type t =
      { label : string
      ; value : float
      }
    [@@deriving sexp_of]
  end

  module Data : sig
    type t =
      | Categories of
          { labels : string list
          ; series : Series.t list
          } (** line, area and the bars *)
      | Slices of Slice.t list (** pie, donut *)
      | Points of Points.t list (** scatter *)
    [@@deriving sexp_of]
  end

  type t =
    { kind : Kind.t
    ; title : string option
    ; subtitle : string option
    ; unit : string option
    ; x_label : string option
    ; y_label : string option
    ; height : Height.t
    ; data : Data.t
    }
  [@@deriving sexp_of]
end

module Calculator : sig
  module Input : sig
    type t =
      { name : string
      ; label : string
      ; default : float
      ; min : float option
      ; max : float option
      ; step : float option
      ; unit : string option
      ; slider : bool
      }
    [@@deriving sexp_of]
  end

  module Format : sig
    type t =
      | Number
      | Percent
      | Currency
      | Integer
    [@@deriving sexp_of, equal]
  end

  module Output : sig
    type t =
      { label : string
      ; expr : string
      ; format : Format.t
      ; decimals : int option
      ; unit : string option
      }
    [@@deriving sexp_of]
  end

  type t =
    { title : string option
    ; inputs : Input.t list
    ; outputs : Output.t list
    }
  [@@deriving sexp_of]
end

module Field : sig
  module Kind : sig
    type t =
      | Text
      | Textarea
      | Number
      | Select
      | Radio
      | Checkbox
      | Toggle
      | Slider
      | Date
    [@@deriving sexp_of, equal]
  end

  type t =
    { name : string
    ; label : string
    ; kind : Kind.t
    ; options : string list
    ; default : Json.t option
    ; min : float option
    ; max : float option
    ; step : float option
    ; required : bool
    ; placeholder : string option
    ; help : string option
    }
  [@@deriving sexp_of]
end

module Item : sig
  (** An entry of a timeline, a steps list, a tree, ... : what each needs. *)
  type t =
    { title : string
    ; time : string option
    ; text : string option (** markdown *)
    ; tone : Tone.t
    }
  [@@deriving sexp_of]
end

module Step_status : sig
  type t =
    | Done
    | Current
    | Todo
    | Failed
  [@@deriving sexp_of, equal]
end

module Image_width : sig
  type t =
    | Small
    | Medium
    | Full
  [@@deriving sexp_of, equal]
end

module Image : sig
  type t =
    { path : string
    ; caption : string option (** markdown *)
    ; alt : string option
    }
  [@@deriving sexp_of]
end

module Choice : sig
  type t =
    { label : string
    ; value : string (** the label when not given *)
    ; description : string option
    ; tone : Tone.t
    }
  [@@deriving sexp_of]
end

module Question : sig
  type t =
    { question : string (** markdown *)
    ; options : string list
    ; answer : int (** 0-based *)
    ; explanation : string option (** markdown *)
    }
  [@@deriving sexp_of]
end

module Trend : sig
  type t =
    | Up
    | Down
    | Flat
  [@@deriving sexp_of, equal]
end

module Component : sig
  type t =
    { id : string option
    ; body : body
    }

  and body =
    | Card of
        { title : string option
        ; subtitle : string option
        ; children : t list
        }
    | Grid of
        { columns : int (** 1–4 *)
        ; children : t list
        }
    | Tabs of (string * t list) list
    | Accordion of
        { items : accordion_item list }
    | Divider
    | Markdown of string
    | Callout of
        { tone : Tone.t
        ; title : string option
        ; text : string
        }
    | Stat of
        { label : string
        ; value : Scalar.t
        ; unit : string option
        ; delta : Scalar.t option
        ; trend : Trend.t option
        ; good : Trend.t option (** which trend is good: [Up] or [Down] *)
        ; caption : string option
        ; spark : float list
        }
    | Kv of
        { title : string option
        ; items : (string * string) list (** values are markdown *)
        }
    | Progress of
        { label : string option
        ; value : float
        ; max : float
        ; tone : Tone.t
        ; caption : string option
        }
    | Badges of (string * Tone.t) list
    | Table of Table.t
    | Chart of Chart.t
    | Timeline of
        { title : string option
        ; items : Item.t list
        }
    | Steps of
        { title : string option
        ; items : (Item.t * Step_status.t) list
        }
    | Code of
        { lang : string option
        ; title : string option
        ; code : string
        }
    | Diff of
        { title : string option
        ; diff : string
        }
    | Tree of
        { title : string option
        ; entries : (string * string option * Tone.t) list
          (** path (directories end in [/]), note, tone *)
        }
    | Image of
        { image : Image.t
        ; width : Image_width.t
        }
    | Gallery of
        { title : string option
        ; images : Image.t list
        }
    | Calculator of Calculator.t
    | Flashcards of
        { title : string option
        ; cards : (string * string) list (** front, back *)
        }
    | Quiz of
        { title : string option
        ; questions : Question.t list
        }
    | Form of
        { title : string option
        ; description : string option
        ; fields : Field.t list
        ; submit : string
        }
    | Choices of
        { prompt : string option
        ; options : Choice.t list
        ; multiple : bool
        }
    | Checklist of
        { title : string option
        ; items : (string * bool) list
        ; submit : string option
        }
    | Unknown of
        { type_ : string
        ; raw : Json.t
        }

  and accordion_item =
    { title : string
    ; children : t list
    ; open_ : bool
    }
  [@@deriving sexp_of]

  (** The component's [type] on the wire ([chart], [table], ...). *)
  val type_name : t -> string

  (** What a reply through it is called: its title, prompt or id. *)
  val title : t -> string option

  (** Its children (layouts), in order. *)
  val children : t -> t list
end

type t = Component.t list [@@deriving sexp_of]

(** [{"components": [...]}]. An error only when there is no list of
    components. *)
val of_json : Json.t -> t Or_error.t

(** A call's arguments as JSON text; [None] while they do not parse (still
    streaming) or have no components. *)
val of_arguments : string -> t option

(** One line per top-level component: [chart: Build times · 3 series],
    [table: Slowest tests · 12 rows], [image: /home/u/shot.png],
    [form: Deploy options]. *)
val summary : t -> string list

(** Every component, depth first, with its path ([0], [1.0], [1.0.2]: the
    indices of it and its ancestors, tabs and accordion items counting as
    one level each). *)
val walk : t -> (string * Component.t) list

(** The paths of the images the components show, in order. *)
val image_paths : t -> string list
