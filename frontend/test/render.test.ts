import assert from "node:assert/strict";
import { test } from "node:test";
import { formatTokens, renderMessage, renderStatus, rowsFor, summariseArguments } from "../src/tui/render.js";

const plain = { color: false, width: 40 };

test("renderMessage without color", () => {
	assert.equal(renderMessage({ role: "user", text: "hi\nthere" }, plain), "> hi\n> there");
	assert.equal(
		renderMessage(
			{
				role: "assistant",
				content: [
					{ type: "thinking", text: "hmm" },
					{ type: "text", text: "Sure." },
					{ type: "tool_call", id: "1", name: "bash", arguments: '{"command":"ls -la"}' },
				],
				stop_reason: { type: "error", message: "boom" },
				usage: { input: 0, output: 0, cache_read: 0 },
				model: "m",
			},
			plain,
		),
		"  hmm\nSure.\n⚙ bash command=ls -la\nerror: boom",
	);
	assert.equal(
		renderMessage({ role: "tool_result", tool_call_id: "1", tool_name: "bash", text: "a\nb\nc\n", is_error: true }, plain),
		"  a\n  b\n  c",
	);
});

test("long tool results are elided", () => {
	const text = Array.from({ length: 20 }, (_, i) => `line${i}`).join("\n");
	const out = renderMessage({ role: "tool_result", tool_call_id: "1", tool_name: "read", text, is_error: false }, plain);
	assert.ok(out.endsWith("… (12 more lines)"));
});

test("summariseArguments", () => {
	assert.equal(summariseArguments({ id: "1", name: "edit", arguments: '{"path":"a.ml","edits":[{"old_text":"x"}]}' }), 'path=a.ml edits=[{"old_text":"x"}]');
	assert.equal(summariseArguments({ id: "1", name: "x", arguments: "not json" }), "not json");
	assert.ok(summariseArguments({ id: "1", name: "x", arguments: JSON.stringify({ content: "y".repeat(200) }) }).endsWith("..."));
});

test("status line and helpers", () => {
	assert.equal(formatTokens(999), "999");
	assert.equal(formatTokens(12_345), "12.3k");
	assert.equal(formatTokens(2_500_000), "2.5M");
	assert.equal(renderStatus(null, "", plain), "connecting…");
	const status = renderStatus(
		{
			session_id: "s",
			session_path: "/p",
			cwd: "/c",
			model: { id: "deepseek-flash", provider: "deepseek", key: "deepseek/deepseek-flash", name: "", context_window: 1000, max_output: 1, supports_thinking: true, cost: { input: 0, output: 0, cache_read: 0 } },
			thinking: "high",
			running: true,
			message_count: 3,
			usage: { input: 1500, output: 20, cache_read: 0 },
			cost_usd: 0.01234,
			context_tokens: 500,
		},
		"working",
		{ color: false, width: 200 },
	);
	assert.equal(status, "deepseek/deepseek-flash  thinking:high  ctx:500 (50%)  in:1.5k out:20  $0.0123  working");
	assert.equal(rowsFor("a\n\x1b[31m" + "b".repeat(45) + "\x1b[0m", 40), 3);
});

test("formatAuthStatus", async () => {
	const { formatAuthStatus } = await import("../src/tui/app.js");
	const text = formatAuthStatus([
		{ provider: "anthropic", name: "Anthropic", methods: [{ method: "oauth", label: "Anthropic (Claude Pro/Max)" }, { method: "api_key", label: "Anthropic API key" }], configured: { method: "oauth", source: "oauth" }, expires_ms: 1 },
		{ provider: "deepseek", name: "DeepSeek", methods: [{ method: "api_key", label: "DeepSeek API key" }], configured: null, expires_ms: null },
	]);
	const plain = text.replace(/\x1b\[[0-9;]*m/g, "");
	assert.equal(
		plain,
		["anthropic  logged in via oauth  [oauth (Anthropic (Claude Pro/Max)), api_key (Anthropic API key)]", "deepseek   not configured  [api_key (DeepSeek API key)]"].join("\n"),
	);
});
