import assert from "node:assert/strict";
import { test } from "node:test";
import { completeCommand, helpText, parseCommand } from "../src/tui/commands.js";

test("parseCommand", () => {
	assert.equal(parseCommand("hello"), null);
	assert.deepEqual(parseCommand("/model  deepseek-flash "), { name: "model", args: ["deepseek-flash"], rest: "deepseek-flash" });
	assert.deepEqual(parseCommand("/switch /a path/with spaces.jsonl"), { name: "switch", args: ["/a", "path/with", "spaces.jsonl"], rest: "/a path/with spaces.jsonl" });
	assert.equal(parseCommand("/"), null);
});

test("completeCommand", () => {
	assert.equal(completeCommand("/mo"), "/model ");
	assert.equal(completeCommand("/s"), null); // sessions, switch, state: no progress possible
	assert.equal(completeCommand("/se"), "/sessions ");
	assert.equal(completeCommand("/logi"), "/login ");
	assert.equal(completeCommand("/zz"), null);
	assert.equal(completeCommand("hello"), null);
	assert.equal(completeCommand("/model x"), null);
});

test("helpText lists every command", () => {
	const text = helpText();
	for (const name of ["help", "model", "thinking", "login", "logout", "auth", "compact", "quit"]) assert.ok(text.includes(`/${name}`));
});
