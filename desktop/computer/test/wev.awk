# wev's stdout: buttons (once: wev binds the pointer twice), keys as pressed
# with the modifiers held (once: on Hyprland wev binds the keyboard 4 times);
# its WAYLAND_DEBUG log: wheel steps.
FNR == 1 { file++ }
file == 1 && /wl_pointer\] (enter|motion)/ {
	if (!ptr) ptr = $2
	if ($2 == ptr) { p = $0; sub(/.*x, y: /, "", p); gsub(/\.000000/, "", p) }
}
file == 1 && /wl_pointer\] button/ && $2 == ptr {
	match($0, /\([a-z]+\), state: [0-9] \([a-z]+\)/)
	b = substr($0, RSTART, RLENGTH); gsub(/[(),]|state: [0-9] /, "", b)
	print "button " b " at " p
}
file == 1 && /wl_keyboard\] (key|modifiers):/ { if (!kbd) kbd = $2; mine = $2 == kbd }
file == 1 && /wl_keyboard\] key:/ { pressed = mine && ($0 ~ /pressed/) }
file == 1 && /^ *depressed:/ && mine { mods = $2 == "00000000" ? "" : $3 "+" }
file == 1 && /^ *sym:/ && pressed { print "key " mods $2; pressed = 0 }
file == 2 && /wl_pointer#[0-9]+\.axis_value120\(/ {
	match($0, /wl_pointer#[0-9]+\./); id = substr($0, RSTART, RLENGTH)
	if (!dptr) dptr = id
	if (id == dptr) { v = $0; sub(/.*axis_value120/, "", v); print "wheel (axis, value120) " v }
}
