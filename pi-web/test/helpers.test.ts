import { describe, expect, it } from "vitest";
import { buildChatEntries } from "../src/chat-items.ts";
import { formatRelativeTime, sessionTitle } from "../src/sessions.ts";
import { diffLineCount, editsOf, summarizeArgs } from "../src/tool-args.ts";
import type { PrighSession } from "../src/protocol.ts";

const session = (overrides: Partial<PrighSession>): PrighSession => ({
	id: "0123456789abcdef",
	path: "/s.jsonl",
	name: null,
	description: null,
	cwd: "/",
	created_at: "2025-01-01 00:00:00.000Z",
	updated_at: "2025-01-01 00:00:00.000Z",
	first_prompt: null,
	message_count: 0,
	parent: null,
	live: false,
	...overrides,
});

describe("sessionTitle", () => {
	it("prefers the name, then the description, then the first prompt, then the id", () => {
		expect(sessionTitle(session({ name: "named", description: "desc", first_prompt: "hi" }))).toBe("named");
		expect(sessionTitle(session({ description: "desc", first_prompt: "hi" }))).toBe("desc");
		expect(sessionTitle(session({ first_prompt: "hi\nthere" }))).toBe("hi");
		expect(sessionTitle(session({}))).toBe("0123456789abcdef");
	});

	it("truncates long titles", () => {
		expect(sessionTitle(session({ name: "x".repeat(100) }))).toBe(`${"x".repeat(77)}…`);
	});
});

describe("formatRelativeTime", () => {
	const now = Date.parse("2025-01-02T00:00:00Z");
	it("buckets by unit", () => {
		expect(formatRelativeTime("2025-01-01T23:59:30Z", now)).toBe("just now");
		expect(formatRelativeTime("2025-01-01T23:50:00Z", now)).toBe("10m ago");
		expect(formatRelativeTime("2025-01-01T20:00:00Z", now)).toBe("4h ago");
		expect(formatRelativeTime("2024-12-30T00:00:00Z", now)).toBe("3d ago");
		expect(formatRelativeTime("nonsense", now)).toBeUndefined();
		expect(formatRelativeTime(undefined, now)).toBeUndefined();
	});
});

describe("tool argument helpers (prigh's tool shapes)", () => {
	it("reads prigh's edits list", () => {
		expect(editsOf({ path: "a", edits: [{ old_text: "x", new_text: "y\nz" }, { bogus: 1 }] })).toEqual([
			{ old_text: "x", new_text: "y\nz" },
		]);
		expect(editsOf({ path: "a" })).toEqual([]);
	});

	it("counts diff lines for edit and write", () => {
		expect(diffLineCount("edit", { edits: [{ old_text: "x", new_text: "y\nz" }, { old_text: "a", new_text: "b" }] })).toBe(3);
		expect(diffLineCount("write", { path: "f", content: "1\n2\n3" })).toBe(3);
		expect(diffLineCount("bash", { command: "ls" })).toBe(0);
	});

	it("summarises the most telling argument", () => {
		expect(summarizeArgs({ command: "ls   -la" })).toBe("ls -la");
		expect(summarizeArgs({ path: "src/a.ml", edits: [] })).toBe("src/a.ml");
		expect(summarizeArgs({ pattern: "foo" })).toBe("foo");
		expect(summarizeArgs({ task: "look\naround" })).toBe("look around");
		expect(summarizeArgs({ other: 1 })).toBe('{"other":1}');
		expect(summarizeArgs({ command: "x".repeat(120) })).toBe(`${"x".repeat(100)}…`);
	});
});

describe("buildChatEntries", () => {
	it("keeps user/assistant/custom entries and drops tool results", () => {
		const entries = buildChatEntries([
			{ role: "user", content: "hi", timestamp: 0 },
			{ role: "assistant", content: [], provider: "prigh", model: "m", stopReason: "stop", timestamp: 1 },
			{ role: "toolResult", toolCallId: "c", toolName: "ls", content: [], isError: false, timestamp: 2 },
			{ role: "custom", customType: "help", content: "x", display: true, timestamp: 3 },
			{ role: "custom", customType: "hidden", content: "x", display: false, timestamp: 4 },
		]);
		expect(entries.map((entry) => `${entry.kind}:${entry.key}`)).toEqual([
			"user:user:0",
			"assistant:assistant:1",
			"custom:custom:3",
		]);
	});
});
