import assert from "node:assert/strict";
import { test } from "node:test";
import { isAuthStatus, isEvent, isMessage, isState, parseServerMessage } from "../src/protocol.js";

const state = {
	session_id: "s",
	session_path: "/p",
	cwd: "/c",
	model: { id: "m", provider: "deepseek", key: "deepseek/m", name: "M", context_window: 10, max_output: 5, supports_thinking: true, cost: { input: 1, output: 2, cache_read: 0 } },
	thinking: "off",
	running: false,
	message_count: 0,
	usage: { input: 0, output: 0, cache_read: 0 },
	cost_usd: 0,
	context_tokens: 0,
};

test("message guards", () => {
	assert.ok(isMessage({ role: "user", text: "hi" }));
	assert.ok(!isMessage({ role: "user" }));
	assert.ok(
		isMessage({
			role: "assistant",
			content: [{ type: "text", text: "x" }, { type: "tool_call", id: "1", name: "ls", arguments: "{}" }],
			stop_reason: { type: "error", message: "m" },
			usage: { input: 1, output: 2, cache_read: 3 },
			model: "m",
		}),
	);
	assert.ok(!isMessage({ role: "assistant", content: [{ type: "bogus" }], stop_reason: { type: "end_turn" }, usage: { input: 1, output: 2, cache_read: 3 }, model: "m" }));
	assert.ok(isMessage({ role: "tool_result", tool_call_id: "1", tool_name: "ls", text: "", is_error: false }));
});

test("state and event guards", () => {
	assert.ok(isState(state));
	assert.ok(!isState({ ...state, thinking: "sideways" }));
	assert.ok(isEvent({ event: "state", state }));
	assert.ok(isEvent({ event: "message_update", partial: { role: "assistant", content: [], stop_reason: { type: "end_turn" }, usage: state.usage, model: "m" }, delta: { type: "text_delta", text: "a" } }));
	assert.ok(isEvent({ event: "tool_output", call_id: "c", chunk: "x" }));
	assert.ok(!isEvent({ event: "tool_output", call_id: "c" }));
	assert.ok(!isEvent({ event: "made_up" }));
});

test("parseServerMessage", () => {
	assert.deepEqual(parseServerMessage('{"type":"response","id":1,"ok":true,"result":"pong"}'), {
		kind: "message",
		message: { type: "response", id: 1, ok: true, result: "pong" },
	});
	assert.deepEqual(parseServerMessage('{"type":"response","id":1,"ok":false,"error":"bad"}'), {
		kind: "message",
		message: { type: "response", id: 1, ok: false, error: "bad" },
	});
	assert.equal(parseServerMessage("{").kind, "invalid");
	assert.equal(parseServerMessage('{"type":"event","event":"nope"}').kind, "invalid");
	assert.equal(parseServerMessage('{"type":"response","id":1}').kind, "invalid");
	const event = parseServerMessage('{"type":"event","event":"notice","text":"hi"}');
	assert.equal(event.kind, "message");
});

test("auth event and status guards", () => {
	assert.ok(isEvent({ event: "auth", kind: "auth_url", url: "https://x", instructions: "go" }));
	assert.ok(isEvent({ event: "auth", kind: "prompt", id: "p1", prompt: "secret", message: "key?" }));
	assert.ok(isEvent({ event: "auth", kind: "prompt", id: "p1", prompt: "manual_code", message: "m", placeholder: "http://localhost" }));
	assert.ok(isEvent({ event: "auth", kind: "prompt", id: "p1", prompt: "select", message: "m", options: [{ id: "a", label: "A" }] }));
	assert.ok(!isEvent({ event: "auth", kind: "prompt", id: "p1", prompt: "select", message: "m", options: [{ id: 1 }] }));
	assert.ok(isEvent({ event: "auth", kind: "prompt_cancelled", id: "p1" }));
	assert.ok(isEvent({ event: "auth", kind: "done", provider: "anthropic", method: "oauth" }));
	assert.ok(!isEvent({ event: "auth", kind: "done", provider: "anthropic", method: "magic" }));
	assert.ok(isEvent({ event: "auth", kind: "failed", provider: "anthropic", error: "e" }));
	assert.ok(isEvent({ event: "auth", kind: "logged_out", provider: "anthropic" }));
	assert.ok(!isEvent({ event: "auth", kind: "bogus" }));
	assert.ok(
		isAuthStatus({
			provider: "anthropic",
			name: "Anthropic",
			methods: [{ method: "oauth", label: "Anthropic (Claude Pro/Max)" }],
			configured: { method: "oauth", source: "oauth" },
			expires_ms: 1,
		}),
	);
	assert.ok(isAuthStatus({ provider: "openai", name: "OpenAI", methods: [], configured: null, expires_ms: null }));
	assert.ok(!isAuthStatus({ provider: "openai", name: "OpenAI", methods: [], configured: { method: "x", source: "s" }, expires_ms: null }));
	assert.ok(isEvent({ event: "message_update", partial: { role: "assistant", content: [], stop_reason: { type: "end_turn" }, usage: state.usage, model: "m" }, delta: { type: "thinking_signature" } }));
});
