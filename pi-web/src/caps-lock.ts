/**
 * Whether Caps Lock is on according to a keyboard (or focus) event. Focus
 * events carry no modifier state, so they report `undefined` (unknown): the
 * caller keeps what it knew from the last key.
 */
export function capsLockFromEvent(event: Event): boolean | undefined {
	const withModifiers = event as Partial<Pick<KeyboardEvent, "getModifierState">>;
	if (typeof withModifiers.getModifierState !== "function") return undefined;
	return withModifiers.getModifierState("CapsLock");
}
