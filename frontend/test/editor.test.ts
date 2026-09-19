import assert from "node:assert/strict";
import { test } from "node:test";
import { Editor } from "../src/tui/editor.js";

test("insert, move, delete across lines", () => {
	const e = new Editor();
	e.insert("hello");
	e.newline();
	e.insert("world");
	assert.equal(e.text, "hello\nworld");
	assert.deepEqual(e.position, { line: 1, col: 5 });
	e.home();
	e.backspace(); // joins lines
	assert.equal(e.text, "helloworld");
	assert.deepEqual(e.position, { line: 0, col: 5 });
	e.insert("\n");
	e.up();
	e.end();
	e.deleteForward();
	assert.equal(e.text, "helloworld");
	assert.deepEqual(e.position, { line: 0, col: 5 });
	e.left();
	e.left();
	e.killToEnd();
	assert.equal(e.text, "hel");
	e.killWord();
	assert.equal(e.text, "");
});

test("multi-line paste positions the cursor at the end of the pasted text", () => {
	const e = new Editor();
	e.insert("ab");
	e.left();
	e.insert("1\n2\n3");
	assert.equal(e.text, "a1\n2\n3b");
	assert.deepEqual(e.position, { line: 2, col: 1 });
	assert.equal(e.lineCount, 3);
});

test("history navigation keeps the draft", () => {
	const e = new Editor();
	e.insert("first");
	assert.equal(e.submit(), "first");
	e.insert("second");
	e.submit();
	e.insert("draft");
	assert.ok(e.historyPrev());
	assert.equal(e.text, "second");
	assert.ok(e.historyPrev());
	assert.equal(e.text, "first");
	assert.ok(!e.historyPrev());
	e.historyNext();
	e.historyNext();
	assert.equal(e.text, "draft");
	assert.ok(!e.historyNext());
	assert.ok(!e.up());
});
