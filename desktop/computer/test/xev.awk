# xev's output: buttons and keys as pressed, with the position and the
# modifier state.
/^(ButtonPress|KeyPress) / { kind = $1; n = NR }
n && NR == n + 1 { pos = $0; sub(/.*root:\(/, "", pos); sub(/\).*/, "", pos) }
n && NR == n + 2 {
	s = $2; sub(/,/, "", s)
	if (kind == "ButtonPress") { b = $4; sub(/,/, "", b); print "button " b " at " pos " state " s }
	else { k = $0; sub(/.*keysym 0x[0-9a-f]+, /, "", k); sub(/\).*/, "", k); print "key " k " state " s }
	n = 0
}
