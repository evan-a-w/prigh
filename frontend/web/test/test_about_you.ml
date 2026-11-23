open! Core
module H = Harness

(* Settings' About you: the text the model is given in every session. *)

let section h = H.text h ~selector:".settings-panel"

let limits ?(short = 100) ?(total = 5000) () =
  sprintf
    {|"limits":{"name":%d,"background":1000,"anything_else":4000,"total":%d}|}
    short
    total
;;

(* As the backend says it: the form, and the block it renders. *)
let info ?short ?total ?(form = {|{}|}) ?(text = "") () =
  sprintf
    {|{"form":%s,"text":%s,%s}|}
    form
    (Jsonaf.to_string (`String text))
    (limits ?short ?total ())
;;

let lloyd =
  info
    ~form:{|{"name":"Lloyd","units":"metric"}|}
    ~text:"- Name: Lloyd\n- Units: metric (°C, km, kg, L)"
    ()
;;

let opened ?(info = lloyd) () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Settings_section About_you);
    H.reply h "get_user_info" info);
  h
;;

let changed h info =
  H.event
    h
    (String.substr_replace_first
       info
       ~pattern:"{"
       ~with_:{|{"type":"event","event":"user_info_changed",|})
;;

let%expect_test "opening reads it; the form's fields; what it is for" =
  let h = H.create () in
  H.act h (Settings_section About_you);
  section h;
  H.reply h "get_user_info" (info ());
  section h;
  H.show h ~selector:".about-you-form";
  H.text h ~selector:".settings-nav";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    About you
    What the model should know about you, given to it in every session. Kept on the backend, for your account.
    Tell the model about you Every field is optional. What you fill in is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    Name
    []
    Country or region
    []
    Time zone
    []
    Language and spelling
    []
    Units
    (Not set) (Metric) (Imperial) (US)
    Date format
    (Not set) (D/M/Y) (M/D/Y) (ISO)
    Occupation or role
    []
    Background and expertise
    []
    Anything else
    []
    Reading what you set…
    Ctrl+Enter saves (Save)
    About you
    What the model should know about you, given to it in every session. Kept on the backend, for your account.
    Tell the model about you Every field is optional. What you fill in is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    Name
    []
    Country or region
    []
    Time zone
    []
    Language and spelling
    []
    Units
    (Not set) (Metric) (Imperial) (US)
    Date format
    (Not set) (D/M/Y) (M/D/Y) (ISO)
    Occupation or role
    []
    Background and expertise
    []
    Anything else
    []
    0 of 5000 bytes Ctrl+Enter saves (Save)
    <div class="about-you-form flex flex-wrap gap-x-4 gap-y-3 items-start">
      <div id="about-you-name-field"
           class="about-you-field flex flex-[1_1_220px] flex-col gap-1.5 min-w-0">
        <label for="about-you-name"
               class="about-you-label font-medium text-[13px] text-muted-foreground"> Name </label>
        <input id="about-you-name"
               maxlength="100"
               type="text"
               placeholder="What to call you"
               autocomplete="off"
               class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-country-field"
           class="about-you-field flex flex-[1_1_220px] flex-col gap-1.5 min-w-0">
        <label for="about-you-country"
               class="about-you-label font-medium text-[13px] text-muted-foreground"> Country or region </label>
        <input id="about-you-country"
               maxlength="100"
               type="text"
               placeholder="Australia"
               autocomplete="off"
               class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-time-zone-field"
           class="about-you-field flex flex-[1_1_220px] flex-col gap-1.5 min-w-0">
        <label for="about-you-time-zone"
               class="about-you-label font-medium text-[13px] text-muted-foreground"> Time zone </label>
        <input id="about-you-time-zone"
               maxlength="100"
               type="text"
               placeholder="Australia/Sydney"
               autocomplete="off"
               class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-language-field"
           class="about-you-field flex flex-[1_1_220px] flex-col gap-1.5 min-w-0">
        <label for="about-you-language"
               class="about-you-label font-medium text-[13px] text-muted-foreground"> Language and spelling </label>
        <input id="about-you-language"
               maxlength="100"
               type="text"
               placeholder="English, Australian spelling"
               autocomplete="off"
               class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-units-field"
           class="about-you-field flex flex-[1_1_220px] flex-col gap-1.5 min-w-0">
        <label class="about-you-label font-medium text-[13px] text-muted-foreground"> Units </label>
        <div id="about-you-units" class="about-you-choice min-w-0">
          <div class="loon max-w-full min-w-0 setting-segments">
            <div role="radiogroup"
                 dir="ltr"
                 data-slot="toggle-group"
                 tabindex="0"
                 data-variant="outline"
                 data-size="sm"
                 data-spacing="2"
                 data-orientation="horizontal"
                 aria-label="Units"
                 class="data-[size=sm]:rounded-[min(var(--radius-md),10px)] data-vertical:flex-col data-vertical:items-stretch flex flex-row flex-wrap gap-[--spacing(var(--gap))] group/toggle-group items-center max-w-full narrow:w-full rounded-lg w-fit"
                 custom-css-vars=((--gap 2))
                 @on_focusout
                 @on_focus>
              <button type="button"
                      data-state="on"
                      role="radio"
                      aria-checked="true"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="Not set"
                      title="Leave it out"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 on outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> Not set </button>
              <button type="button"
                      data-state="off"
                      role="radio"
                      aria-checked="false"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="Metric"
                      title="Metric (°C, km, kg)"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> Metric </button>
              <button type="button"
                      data-state="off"
                      role="radio"
                      aria-checked="false"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="Imperial"
                      title="Imperial (miles, stone; °C)"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> Imperial </button>
              <button type="button"
                      data-state="off"
                      role="radio"
                      aria-checked="false"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="US"
                      title="US customary (°F, miles, lb)"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> US </button>
            </div>
          </div>
        </div>
      </div>
      <div id="about-you-date-format-field"
           class="about-you-field flex flex-[1_1_220px] flex-col gap-1.5 min-w-0">
        <label class="about-you-label font-medium text-[13px] text-muted-foreground"> Date format </label>
        <div id="about-you-date-format" class="about-you-choice min-w-0">
          <div class="loon max-w-full min-w-0 setting-segments">
            <div role="radiogroup"
                 dir="ltr"
                 data-slot="toggle-group"
                 tabindex="0"
                 data-variant="outline"
                 data-size="sm"
                 data-spacing="2"
                 data-orientation="horizontal"
                 aria-label="Date format"
                 class="data-[size=sm]:rounded-[min(var(--radius-md),10px)] data-vertical:flex-col data-vertical:items-stretch flex flex-row flex-wrap gap-[--spacing(var(--gap))] group/toggle-group items-center max-w-full narrow:w-full rounded-lg w-fit"
                 custom-css-vars=((--gap 2))
                 @on_focusout
                 @on_focus>
              <button type="button"
                      data-state="on"
                      role="radio"
                      aria-checked="true"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="Not set"
                      title="Leave it out"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 on outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> Not set </button>
              <button type="button"
                      data-state="off"
                      role="radio"
                      aria-checked="false"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="D/M/Y"
                      title="D/M/Y (25/12/2026)"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> D/M/Y </button>
              <button type="button"
                      data-state="off"
                      role="radio"
                      aria-checked="false"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="M/D/Y"
                      title="M/D/Y (12/25/2026)"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> M/D/Y </button>
              <button type="button"
                      data-state="off"
                      role="radio"
                      aria-checked="false"
                      tabindex="-1"
                      data-slot="toggle-group-item"
                      data-variant="outline"
                      data-size="sm"
                      data-spacing="2"
                      aria-label="ISO"
                      title="ISO (2026-12-25)"
                      class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                      @on_click
                      @on_focus
                      @on_keydown> ISO </button>
            </div>
          </div>
        </div>
      </div>
      <div id="about-you-occupation-field"
           class="about-you-field flex flex-[1_1_220px] flex-col gap-1.5 min-w-0">
        <label for="about-you-occupation"
               class="about-you-label font-medium text-[13px] text-muted-foreground"> Occupation or role </label>
        <input id="about-you-occupation"
               maxlength="100"
               type="text"
               placeholder="Backend engineer"
               autocomplete="off"
               class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-background-field"
           class="about-you-field basis-full flex flex-col gap-1.5 min-w-0 wide">
        <label for="about-you-background"
               class="about-you-label font-medium text-[13px] text-muted-foreground"> Background and expertise </label>
        <textarea id="about-you-background"
                  maxlength="1000"
                  placeholder="Undergrad pure maths and CS; years of OCaml"
                  rows="3"
                  class="about-you-input aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 field-sizing-content flex focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 md:text-sm min-h-[4.5em] narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-2 resize-y rounded-lg text-base transition-colors w-full"
                  #value=""
                  @on_input> </textarea>
      </div>
      <div id="about-you-anything-else-field"
           class="about-you-field basis-full flex flex-col gap-1.5 min-w-0 wide">
        <label for="about-you-anything-else"
               class="about-you-label font-medium text-[13px] text-muted-foreground"> Anything else </label>
        <textarea id="about-you-anything-else"
                  maxlength="4000"
                  placeholder="Keep answers short, and ask before adding a dependency."
                  rows="5"
                  class="about-you-input aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 field-sizing-content flex focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 md:text-sm min-h-[4.5em] narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-2 resize-y rounded-lg text-base transition-colors w-full"
                  #value=""
                  @on_input> </textarea>
      </div>
    </div>
    General
    (Appearance) (Sessions) (New sessions)
    Models
    (Models) (Usage)
    You
    (Memory) (About you) (Custom instructions)
    Integrations
    (MCP servers) (Git hosting)
    (Account)
    |}]
;;

let%expect_test "the browser's time zone is a tap away" =
  let h = opened () in
  H.act h (Set_browser_time_zone "Australia/Sydney");
  H.text h ~selector:"#about-you-time-zone-field";
  H.click h ~selector:".about-you-detect";
  H.show h ~selector:"#about-you-time-zone";
  H.text h ~selector:"#about-you-time-zone-field";
  [%expect
    {|
    Time zone
    [] (Use Australia/Sydney)
    (About_you_field Time_zone Australia/Sydney)
    <input id="about-you-time-zone"
           maxlength="100"
           type="text"
           placeholder="Australia/Sydney"
           autocomplete="off"
           class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
           #value="Australia/Sydney"
           @on_input/>
    Time zone
    [Australia/Sydney]
    |}]
;;

let%expect_test "editing makes it dirty: Save and Revert; Ctrl+Enter saves" =
  let h = opened () in
  H.text h ~selector:".about-you-buttons";
  H.input h ~selector:"#about-you-country" "Australia ";
  H.click h ~selector:"#about-you-date-format .segment:nth-child(2)";
  H.input h ~selector:"#about-you-background" "Pure maths and CS.\n";
  H.text h ~selector:".about-you-buttons";
  H.key h ~ctrl:true ~target:Multiline "Enter";
  H.text h ~selector:".about-you-buttons";
  (* The broadcast of our own save arrives before its reply. *)
  let saved =
    info
      ~form:
        {|{"name":"Lloyd","country":"Australia","units":"metric","date_format":"dmy","background":"Pure maths and CS."}|}
      ~text:"- Name: Lloyd\n- Country/region: Australia\n…"
      ()
  in
  changed h saved;
  H.reply h "set_user_info" saved;
  H.text h ~selector:".about-you";
  [%expect
    {|
    5 of 5000 bytes Ctrl+Enter saves (Save)
    (About_you_field Country "Australia ")
    (About_you_field Date_format dmy)
    (About_you_field Background "Pure maths and CS.\n")
    32 of 5000 bytes Ctrl+Enter saves (Revert) (Save)
    Dialog_accept
    (Rpc (method_ set_user_info)
     (params
      ((form
        ((name Lloyd) (country Australia) (units metric) (date_format dmy)
         (background "Pure maths and CS.")))))
     (tag About_you_saved))
    32 of 5000 bytes Ctrl+Enter saves (Revert) (Saving…)
    Tell the model about you Every field is optional. What you fill in is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    Name
    [Lloyd]
    Country or region
    [Australia]
    Time zone
    []
    Language and spelling
    []
    Units
    (Not set) (Metric) (Imperial) (US)
    Date format
    (Not set) (D/M/Y) (M/D/Y) (ISO)
    Occupation or role
    []
    Background and expertise
    [Pure maths and CS.]
    Anything else
    []
    Saved. New sessions start with it; open ones get it with your next message in them.
    32 of 5000 bytes Ctrl+Enter saves (Save)
    |}];
  H.input h ~selector:"#about-you-name" "someone else";
  H.click h ~selector:"#about-you-units .segment:first-child";
  H.click h ~selector:".about-you-buttons button.ghost";
  H.show h ~selector:"#about-you-name";
  H.show h ~selector:"#about-you-units";
  H.text h ~selector:".about-you-buttons";
  [%expect
    {|
    (About_you_field Name "someone else")
    (About_you_field Units "")
    About_you_revert
    (Focus about-you-name)
    <input id="about-you-name"
           maxlength="100"
           type="text"
           placeholder="What to call you"
           autocomplete="off"
           class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
           #value="Lloyd"
           @on_input/>
    <div id="about-you-units" class="about-you-choice min-w-0">
      <div class="loon max-w-full min-w-0 setting-segments">
        <div role="radiogroup"
             dir="ltr"
             data-slot="toggle-group"
             tabindex="0"
             data-variant="outline"
             data-size="sm"
             data-spacing="2"
             data-orientation="horizontal"
             aria-label="Units"
             class="data-[size=sm]:rounded-[min(var(--radius-md),10px)] data-vertical:flex-col data-vertical:items-stretch flex flex-row flex-wrap gap-[--spacing(var(--gap))] group/toggle-group items-center max-w-full narrow:w-full rounded-lg w-fit"
             custom-css-vars=((--gap 2))
             @on_focusout
             @on_focus>
          <button type="button"
                  data-state="off"
                  role="radio"
                  aria-checked="false"
                  tabindex="-1"
                  data-slot="toggle-group-item"
                  data-variant="outline"
                  data-size="sm"
                  data-spacing="2"
                  aria-label="Not set"
                  title="Leave it out"
                  class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                  @on_click
                  @on_focus
                  @on_keydown> Not set </button>
          <button type="button"
                  data-state="on"
                  role="radio"
                  aria-checked="true"
                  tabindex="-1"
                  data-slot="toggle-group-item"
                  data-variant="outline"
                  data-size="sm"
                  data-spacing="2"
                  aria-label="Metric"
                  title="Metric (°C, km, kg)"
                  class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 on outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                  @on_click
                  @on_focus
                  @on_keydown> Metric </button>
          <button type="button"
                  data-state="off"
                  role="radio"
                  aria-checked="false"
                  tabindex="-1"
                  data-slot="toggle-group-item"
                  data-variant="outline"
                  data-size="sm"
                  data-spacing="2"
                  aria-label="Imperial"
                  title="Imperial (miles, stone; °C)"
                  class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                  @on_click
                  @on_focus
                  @on_keydown> Imperial </button>
          <button type="button"
                  data-state="off"
                  role="radio"
                  aria-checked="false"
                  tabindex="-1"
                  data-slot="toggle-group-item"
                  data-variant="outline"
                  data-size="sm"
                  data-spacing="2"
                  aria-label="US"
                  title="US customary (°F, miles, lb)"
                  class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap"
                  @on_click
                  @on_focus
                  @on_keydown> US </button>
        </div>
      </div>
    </div>
    32 of 5000 bytes Ctrl+Enter saves (Save)
    |}]
;;

let%expect_test "emptying every field clears it" =
  let h = opened () in
  H.input h ~selector:"#about-you-name" "  ";
  H.click h ~selector:"#about-you-units .segment:first-child";
  H.click h ~selector:".about-you-buttons button.primary";
  H.reply h "set_user_info" (info ());
  H.text h ~selector:".about-you-saved";
  [%expect
    {|
    (About_you_field Name "  ")
    (About_you_field Units "")
    About_you_save
    (Rpc (method_ set_user_info) (params ((form ()))) (tag About_you_saved))
    Cleared. Open sessions are told with your next message in them.
    |}]
;;

let%expect_test "refused: the reason stays under the form, which is kept" =
  let h = opened () in
  H.input h ~selector:"#about-you-occupation" "student";
  H.click h ~selector:".about-you-buttons button.primary";
  H.fail
    h
    "set_user_info"
    "cannot write /home/me/.prigh/about_you.json: Permission denied";
  H.text h ~selector:".about-you-error";
  H.text h ~selector:".about-you-buttons";
  [%expect
    {|
    (About_you_field Occupation student)
    About_you_save
    (Rpc (method_ set_user_info)
     (params ((form ((name Lloyd) (units metric) (occupation student)))))
     (tag About_you_saved))
    cannot write /home/me/.prigh/about_you.json: Permission denied
    12 of 5000 bytes Ctrl+Enter saves (Revert) (Save)
    |}];
  (* Over a limit is said here, with nothing sent. *)
  let h =
    opened ~info:(info ~short:10 ~total:30 ~form:{|{"name":"Lloyd"}|} ()) ()
  in
  H.input h ~selector:"#about-you-name" "Lloyd Smith";
  H.click h ~selector:".about-you-buttons button.primary";
  H.text h ~selector:".about-you-error";
  H.input h ~selector:"#about-you-name" "Lloyd";
  H.input h ~selector:"#about-you-background" "Pure maths and CS, and more.";
  H.click h ~selector:".about-you-buttons button.primary";
  H.text h ~selector:".about-you-error";
  H.text h ~selector:".about-you-count";
  [%expect
    {|
    (About_you_field Name "Lloyd Smith")
    About_you_save
    Name is 11 bytes and the limit is 10: shorten it, and keep what is only sometimes needed in memory notes.
    (About_you_field Name Lloyd)
    (About_you_field Background "Pure maths and CS, and more.")
    About_you_save
    All together this is 33 bytes and the limit is 30: shorten it, and keep what is only sometimes needed in memory notes.
    33 of 30 bytes
    |}]
;;

let%expect_test "changed elsewhere: shown at once, unless something is typed" =
  let h = opened () in
  changed h (info ~form:{|{"name":"Elsewhere"}|} ~text:"- Name: Elsewhere" ());
  H.show h ~selector:"#about-you-name";
  H.input h ~selector:"#about-you-name" "Typed here";
  changed h (info ~form:{|{"name":"Third"}|} ~text:"- Name: Third" ());
  H.text h ~selector:".about-you-elsewhere";
  H.click h ~selector:".about-you-elsewhere button";
  H.show h ~selector:"#about-you-name";
  [%expect
    {|
    <input id="about-you-name"
           maxlength="100"
           type="text"
           placeholder="What to call you"
           autocomplete="off"
           class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
           #value="Elsewhere"
           @on_input/>
    (About_you_field Name "Typed here")
    It was changed somewhere else while you were typing. Saving replaces that with this. (Load that one)
    About_you_revert
    (Focus about-you-name)
    <input id="about-you-name"
           maxlength="100"
           type="text"
           placeholder="What to call you"
           autocomplete="off"
           class="about-you-line aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm flex-1 focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
           #value="Third"
           @on_input/>
    |}]
;;

let%expect_test "a failed read says so" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Settings_section About_you));
  H.fail h "get_user_info" "cannot read /home/me/.prigh/about_you.json";
  H.text h ~selector:".about-you > .setting-note";
  [%expect
    {| Could not read it: cannot read /home/me/.prigh/about_you.json. Open the section again to retry. |}]
;;

let%expect_test "/user-info opens the section; it takes no argument" =
  let h = H.create () in
  H.act h (Run "/user-info");
  H.text h ~selector:".settings-panel-head h3";
  H.act h (Run "/user-info I'm Lloyd");
  H.text h ~selector:".toast";
  H.act h (Run "/settings about");
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    About you
    /user-info takes no argument here: it opens Settings, About you, where you fill in the form. (Dismiss)
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    |}]
;;

let%expect_test "the note a session is told with is a margin note" =
  let chat =
    Chat_harness.chat
      [ {|{"event":"message_start","message":{"role":"user","text":"[User info: what the user wrote about themselves, for every session, is now:\nI'm Lloyd.]\n\nhello","origin":{"body":"hello","notes":[{"kind":"user_info","title":"About you: the model was given your details","text":"[User info: what the user wrote about themselves, for every session, is now:\nI'm Lloyd.]"}]}}}|}
      ]
  in
  Chat_harness.text chat;
  [%expect
    {|
    ☺ About you: the model was given your details [User info: what the user wrote about themselves, for every session, is now:
    I'm Lloyd.] hello
    |}]
;;
