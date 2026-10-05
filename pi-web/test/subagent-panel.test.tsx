// @vitest-environment happy-dom
// @vitest-environment-options {"url": "http://127.0.0.1:7789/?session=A"}
import { render } from "preact";
import { act } from "preact/test-utils";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { AgentMessage, SubagentInfo } from "../src/protocol.ts";
import { FakeBackend, FakeSocket, session, settle } from "./fake-backend.ts";

let root: HTMLElement;
let state: typeof import("../src/state.ts");

const info: SubagentInfo = {
	id: "a1",
	label: "look around",
	state: "complete",
	startedAt: Date.parse("2026-10-04T10:00:00"),
	endedAt: Date.parse("2026-10-04T10:01:05"),
	activity: { turnCount: 2, toolCount: 1 },
	task: "look around\nin detail",
	model: "deepseek-flash",
	callId: "p1",
	parentId: null,
	result: null,
};

const transcript: AgentMessage[] = [
	{ role: "user", content: "look around\nin detail", timestamp: 0 },
	{
		role: "assistant",
		content: [
			{ type: "text", text: "Listing." },
			{ type: "toolCall", id: "t1", name: "ls", arguments: { path: "src" } },
		],
		provider: "prigh",
		model: "deepseek-flash",
		stopReason: "toolUse",
		timestamp: 1,
	},
];

const node = (id: string, label: string, children?: unknown[]) => ({
	id,
	kind: "subagent",
	label,
	state: "running",
	startedAt: info.startedAt,
	activity: { turnCount: 1, toolCount: 0 },
	...(children ? { children } : {}),
});

beforeEach(async () => {
	vi.stubGlobal("WebSocket", FakeSocket);
	localStorage.clear();
	localStorage.setItem("prigh-pi-web:user", "evan");
	localStorage.setItem("prigh-pi-web:as-user", "alice");
	history.replaceState(null, "", "/?session=A");
	const backend = new FakeBackend([
		session("A", {
			subagents: [node("a1", "look around", [node("a1/c1", "nested")]) as never],
			transcripts: { a1: { subagent: info, messages: transcript } },
			messages: [
				{ role: "user", content: "delegate", timestamp: 0 },
				{
					role: "assistant",
					content: [{ type: "toolCall", id: "p1", name: "subagent", arguments: { task: "look around" } }],
					provider: "prigh",
					model: "deepseek-flash",
					stopReason: "toolUse",
					timestamp: 1,
				},
			],
		}),
	]);
	backend.allowedAsUsers.add("alice");
	FakeSocket.backend = backend;
	vi.resetModules();
	state = await import("../src/state.ts");
	state.client.start();
	await settle();
	root = document.createElement("div");
	document.body.appendChild(root);
});

afterEach(() => {
	render(null, root);
	root.remove();
	state.client.stop();
	vi.unstubAllGlobals();
});

/** The text of the elements matching `selector`, one per line, their leaves separated by " · ". */
function visible(selector: string): string {
	const leaves = (e: Element): string[] =>
		e.children.length === 0
			? [(e.textContent ?? "").replace(/\s+/g, " ").trim()].filter((t) => t !== "")
			: [...e.children].flatMap(leaves);
	return [...root.querySelectorAll(selector)].map((e) => leaves(e).join(" · ")).join("\n");
}

async function mountApp(): Promise<void> {
	const { App } = await import("../src/app.tsx");
	act(() => {
		render(<App />, root);
	});
}

async function click(element: Element | null): Promise<void> {
	if (!element) throw new Error("nothing to click");
	await act(async () => {
		(element as HTMLElement).click();
		await settle();
	});
}

describe("subagent view in the app", () => {
	it("opens from the subagent tool call, shows meta and the conversation, and goes back to the chat", async () => {
		await mountApp();
		expect(visible(".topbar-user")).toBe("evan as alice");
		await click(root.querySelector(".tool-open-subagent"));
		expect(
			visible(".subagents-panel-header, .subagents-list-item, .subagents-meta-line, .subagents-meta-grid"),
		).toMatchInlineSnapshot(`
				"Subagent a1 · ← Back to chat
				look around · running · a1 · 10:00:00
				nested · running · a1/c1 · 10:00:00
				look around · a1 · complete
				Started · 10:00:00 · Duration · 1m 5s · Model · deepseek-flash · Turns · tools · 2 · 1"
			`);
		expect(visible(".subagent-view .msg")).toMatchInlineSnapshot(`
			"look around in detail
			deepseek-flash · Listing. · ls · src · …"
		`);
		expect(root.querySelector(".editor, textarea")).toBeNull();

		await click(root.querySelector(".subagents-back-to-chat"));
		expect(root.querySelector(".subagent-view")).toBeNull();
		expect(root.querySelector("textarea")).not.toBeNull();
	});

	it("opens from the agents rail, and the list switches between subagents", async () => {
		await mountApp();
		await click(root.querySelector(".agents-rail-node-header"));
		expect(state.subagentView.value?.agentId).toBe("a1");
		expect(root.querySelector(".agents-rail-node.selected .agents-rail-node-label")?.textContent).toBe("look around");
		const nested = [...root.querySelectorAll(".subagents-list-item")].find((e) => e.textContent?.includes("nested"));
		await click(nested ?? null);
		expect(visible(".subagents-list-item.active .subagents-list-item-agent")).toBe("nested");
		expect(visible(".subagents-empty")).toBe(
			'This subagent\'s conversation is not available (unknown subagent "a1/c1"). The backend keeps it in memory only while the session is loaded.',
		);
	});
});
