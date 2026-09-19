import assert from "node:assert/strict";
import { test } from "node:test";
import { parseKeys } from "../src/tui/keys.js";

test("printable, control and escape sequences", () => {
	assert.deepEqual(parseKeys("a"), [{ kind: "char", char: "a" }]);
	assert.deepEqual(parseKeys("\r"), [{ kind: "enter" }]);
	assert.deepEqual(parseKeys("\x1b\r"), [{ kind: "newline" }]);
	assert.deepEqual(parseKeys("\x7f"), [{ kind: "backspace" }]);
	assert.deepEqual(parseKeys("\x03"), [{ kind: "ctrl", letter: "c" }]);
	assert.deepEqual(parseKeys("\x1b[A\x1b[D\x1b[3~\x1b[H\x1bOF"), [
		{ kind: "up" },
		{ kind: "left" },
		{ kind: "delete" },
		{ kind: "home" },
		{ kind: "end" },
	]);
	assert.deepEqual(parseKeys("\x1b"), [{ kind: "escape" }]);
	assert.deepEqual(parseKeys("\x1b[99z"), [{ kind: "unknown", raw: "\x1b[99z" }]);
});

test("pastes", () => {
	assert.deepEqual(parseKeys("\x1b[200~multi\nline\x1b[201~x"), [{ kind: "paste", text: "multi\nline" }, { kind: "char", char: "x" }]);
	assert.deepEqual(parseKeys("hello"), [{ kind: "paste", text: "hello" }]);
});
