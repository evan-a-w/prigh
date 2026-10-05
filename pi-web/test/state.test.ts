// @vitest-environment happy-dom
// @vitest-environment-options {"url": "http://127.0.0.1:7789/?session=A"}
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { AgentMessage, SubagentInfo } from "../src/protocol.ts";
import { FakeBackend, FakeSocket, session, settle } from "./fake-backend.ts";

type State = typeof import("../src/state.ts");

let backend: FakeBackend;
let state: State;

/** Connects a fresh copy of state.ts to `backend`. */
async function start(sessions: FakeBackend): Promise<void> {
	backend = sessions;
	FakeSocket.backend = backend;
	vi.resetModules();
	state = await import("../src/state.ts");
	state.client.start();
	await settle();
}

beforeEach(() => {
	vi.stubGlobal("WebSocket", FakeSocket);
	localStorage.clear();
	history.replaceState(null, "", "/?session=A");
});

afterEach(() => {
	state?.client.stop();
	vi.unstubAllGlobals();
});

const text = (message: AgentMessage) =>
	"content" in message
		? typeof message.content === "string"
			? message.content
			: message.content.map((block) => ("text" in block ? block.text : `[${block.type}]`)).join(" ")
		: message.role;

/** What the session-scoped parts of the UI show. */
function shown(): string {
	const queued = [...state.queue.value.steering, ...state.queue.value.followUp];
	const rail = (state.subagentSnapshot.value?.runs ?? []).map((run) => `${run.id}:${run.state}`);
	const view = state.subagentView.value;
	return [
		`session: ${state.sessionState.value?.sessionId ?? "-"}  url: ${location.search}`,
		`chat: ${state.messages.value.map(text).join(" | ")}`,
		`queued: ${queued.join(", ") || "-"}`,
		`agents rail: ${rail.join(", ") || "-"}`,
		`dialogs: ${state.dialogQueue.value.map((d) => d.id).join(", ") || "-"}`,
		`subagent view: ${
			view
				? `${view.agentId ?? "?"} ${view.status}${view.error ? ` (${view.error})` : ""}: ${view.messages.map(text).join(" | ")}`
				: "-"
		}`,
	].join("\n");
}

const node = (id: string, state: "running" | "complete" = "running") => ({
	id,
	kind: "subagent" as const,
	label: `task of ${id}`,
	state,
	startedAt: 1000,
	activity: { turnCount: 1, toolCount: 0 },
});

const info = (id: string, overrides: Partial<SubagentInfo> = {}): SubagentInfo => ({
	...node(id),
	task: `task of ${id}\nin detail`,
	model: "faux",
	callId: `call-${id}`,
	parentId: null,
	result: null,
	...overrides,
});

describe("per-session state on session changes", () => {
	it("queued messages and the agents rail belong to the session (sidebar switch, /new)", async () => {
		await start(
			new FakeBackend([
				session("A", { steering: ["steer A"], followUp: ["later A"], subagents: [node("a1")] }),
				session("B"),
			]),
		);
		expect(shown()).toMatchInlineSnapshot(`
			"session: A  url: ?session=A
			chat: hello from A
			queued: steer A, later A
			agents rail: a1:running
			dialogs: -
			subagent view: -"
		`);

		await state.switchSession("B");
		await settle();
		expect(shown()).toMatchInlineSnapshot(`
			"session: B  url: ?session=B
			chat: hello from B
			queued: -
			agents rail: -
			dialogs: -
			subagent view: -"
		`);

		await state.switchSession("A");
		await settle();
		expect(shown()).toMatchInlineSnapshot(`
			"session: A  url: ?session=A
			chat: hello from A
			queued: steer A, later A
			agents rail: a1:running
			dialogs: -
			subagent view: -"
		`);

		await state.newSession();
		await settle();
		expect(shown()).toMatchInlineSnapshot(`
			"session: new-2  url: ?session=new-2
			chat: hello from new-2
			queued: -
			agents rail: -
			dialogs: -
			subagent view: -"
		`);
	});

	it("a switch the backend reports (/switch, /sessions) re-syncs the same way", async () => {
		await start(new FakeBackend([session("A", { followUp: ["later A"] }), session("B")]));
		backend.current = "B";
		backend.push({ type: "session_reloaded" });
		await settle();
		expect(shown()).toMatchInlineSnapshot(`
			"session: B  url: ?session=B
			chat: hello from B
			queued: -
			agents rail: -
			dialogs: -
			subagent view: -"
		`);
	});

	it("a reconnect drops what the session no longer has and the old connection's dialogs", async () => {
		await start(
			new FakeBackend([
				session("A", {
					steering: ["steer A"],
					subagents: [node("a1")],
					confirms: [{ id: "confirm-c1", title: "Run bash?", message: "ls" }],
				}),
			]),
		);
		backend.push({ type: "extension_ui_request", id: "dialog-9", method: "select", title: "Pick", options: ["x"] });
		await settle();
		expect(shown()).toMatchInlineSnapshot(`
			"session: A  url: ?session=A
			chat: hello from A
			queued: steer A
			agents rail: a1:running
			dialogs: confirm-c1, dialog-9
			subagent view: -"
		`);

		Object.assign(backend.session, { steering: [], subagents: [node("a1", "complete")] });
		backend.disconnect();
		await settle();
		expect(state.connected.value).toBe(false);
		await vi.waitFor(() => expect(state.connected.value).toBe(true), { timeout: 3000 });
		await settle();
		expect(shown()).toMatchInlineSnapshot(`
			"session: A  url: ?session=A
			chat: hello from A
			queued: -
			agents rail: a1:complete
			dialogs: confirm-c1
			subagent view: -"
		`);
	});

	it("a confirmation the backend asks for again is shown once", async () => {
		await start(
			new FakeBackend([session("A", { confirms: [{ id: "confirm-c1", title: "Run bash?", message: "ls" }] })]),
		);
		await state.sync();
		await settle();
		expect(state.dialogQueue.value.map((d) => d.id)).toEqual(["confirm-c1"]);
	});
});

describe("subagent view", () => {
	const transcript = {
		subagent: info("a1"),
		messages: [
			{ role: "user", content: "task of a1\nin detail", timestamp: 0 },
			{
				role: "assistant",
				content: [{ type: "toolCall", id: "t1", name: "ls", arguments: {} }],
				provider: "prigh",
				model: "faux",
				stopReason: "toolUse",
				timestamp: 1,
			},
		] satisfies AgentMessage[],
	};

	it("opens from the rail, stays live, and closes", async () => {
		await start(
			new FakeBackend([session("A", { subagents: [node("a1")], transcripts: { a1: transcript } }), session("B")]),
		);
		const opening = state.openSubagent({ agentId: "a1" });
		expect(shown()).toContain("subagent view: a1 loading");
		await opening;
		expect(state.toolStates.value).toEqual({});
		expect(state.subagentView.value?.toolStates).toEqual({ t1: { name: "ls", args: {}, status: "running" } });

		backend.pushSubagentEvent("a1", {
			type: "tool_execution_end",
			toolCallId: "t1",
			toolName: "ls",
			result: { content: [{ type: "text", text: "a.txt" }] },
			isError: false,
		});
		backend.pushSubagentEvent("a1", {
			type: "message_update",
			message: {
				role: "assistant",
				content: [{ type: "text", text: "Found a.txt" }],
				provider: "prigh",
				model: "faux",
				stopReason: "stop",
				timestamp: 3,
			},
		});
		backend.pushSubagentEvent("other", {
			type: "message_end",
			message: { role: "user", content: "not ours", timestamp: 4 },
		});
		backend.pushSubagentEvent("a1", { type: "subagent_info", subagent: info("a1", { state: "complete" }) });
		await settle();
		expect(shown()).toMatchInlineSnapshot(`
			"session: A  url: ?session=A
			chat: hello from A
			queued: -
			agents rail: a1:running
			dialogs: -
			subagent view: a1 ready: task of a1
			in detail | [toolCall] | Found a.txt"
		`);
		expect(state.subagentView.value?.info?.state).toBe("complete");
		expect(state.subagentView.value?.toolStates.t1).toMatchObject({ status: "done", output: "a.txt" });

		state.closeSubagent();
		await settle();
		expect(backend.log.filter((line) => line.startsWith("watch_subagent"))).toEqual([
			"watch_subagent a1",
			"watch_subagent",
		]);
		expect(shown()).toContain("subagent view: -");
	});

	it("opens from a subagent tool call (by call id), replaying events that beat the transcript", async () => {
		await start(new FakeBackend([session("A", { transcripts: { a1: transcript } })]));
		const opening = state.openSubagent({ toolCallId: "call-a1" });
		backend.pushSubagentEvent("a1", {
			type: "message_end",
			message: { role: "toolResult", toolCallId: "t1", toolName: "ls", content: [], isError: false, timestamp: 2 },
		});
		await opening;
		await settle();
		expect(state.subagentView.value?.agentId).toBe("a1");
		expect(state.subagentView.value?.messages.map((m) => m.role)).toEqual(["user", "assistant", "toolResult"]);
	});

	it("says so when the backend no longer has the transcript", async () => {
		await start(new FakeBackend([session("A")]));
		await state.openSubagent({ agentId: "a7" });
		expect(shown()).toContain('subagent view: a7 error (unknown subagent "a7")');
	});

	it("closes on a session switch, and is watched again after a reconnect", async () => {
		await start(
			new FakeBackend([session("A", { subagents: [node("a1")], transcripts: { a1: transcript } }), session("B")]),
		);
		await state.openSubagent({ agentId: "a1" });
		backend.disconnect();
		await vi.waitFor(() => expect(backend.urls).toHaveLength(2), { timeout: 3000 });
		await settle();
		expect(backend.log.filter((line) => line.startsWith("watch_subagent"))).toEqual([
			"watch_subagent a1",
			"watch_subagent a1",
		]);
		expect(shown()).toContain("subagent view: a1 ready");

		await state.switchSession("B");
		await settle();
		expect(shown()).toContain("subagent view: -");
	});
});

describe("acting as another user (/setusr)", () => {
	it("remembers the user, drops the session and reconnects with as_user", async () => {
		localStorage.setItem("prigh-pi-web:user", "evan");
		localStorage.setItem("prigh-pi-web:token", "pw");
		const fake = new FakeBackend([session("A")]);
		fake.allowedAsUsers.add("alice");
		await start(fake);
		await state.sendPrompt("/setusr alice", []);
		await settle();
		await vi.waitFor(() => expect(backend.urls).toHaveLength(2));
		await settle();
		expect({
			asUser: state.asUser.value,
			stored: localStorage.getItem("prigh-pi-web:as-user"),
			urls: backend.urls,
			toasts: state.toasts.value.map((t) => t.message),
		}).toMatchInlineSnapshot(`
			{
			  "asUser": "alice",
			  "stored": "alice",
			  "toasts": [
			    "Acting as alice",
			  ],
			  "urls": [
			    "ws://127.0.0.1:7789/ws?user=evan&token=pw&session=A",
			    "ws://127.0.0.1:7789/ws?user=evan&token=pw&as_user=alice",
			  ],
			}
		`);
		expect(state.currentTerminalUrl()).toBe(
			"ws://127.0.0.1:7789/terminal?user=evan&token=pw&as_user=alice&session=A",
		);

		await state.sendPrompt("/setusr evan", []);
		backend.allowedAsUsers.add("evan");
		await settle();
		await state.sendPrompt("/setusr evan", []);
		await vi.waitFor(() => expect(backend.urls).toHaveLength(3));
		expect(state.asUser.value).toBe("");
		expect(localStorage.getItem("prigh-pi-web:as-user")).toBeNull();
		expect(backend.urls[2]).toBe("ws://127.0.0.1:7789/ws?user=evan&token=pw");
		expect(state.toasts.value.map((t) => t.message)).toContain("evan is not a user you may act as");
	});

	it("a refused hello while acting as someone retries once as ourselves", async () => {
		localStorage.setItem("prigh-pi-web:user", "evan");
		localStorage.setItem("prigh-pi-web:as-user", "alice");
		await start(new FakeBackend([session("A")]));
		await vi.waitFor(() => expect(backend.urls).toHaveLength(2));
		await settle();
		expect({
			helloError: state.helloError.value,
			asUser: state.asUser.value,
			stored: localStorage.getItem("prigh-pi-web:as-user"),
			urls: backend.urls,
			toasts: state.toasts.value.map((t) => t.message),
			connected: state.connected.value,
		}).toMatchInlineSnapshot(`
			{
			  "asUser": "",
			  "connected": true,
			  "helloError": undefined,
			  "stored": null,
			  "toasts": [
			    "Could not act as alice (unauthorised: cannot act as alice); back to evan",
			  ],
			  "urls": [
			    "ws://127.0.0.1:7789/ws?user=evan&as_user=alice&session=A",
			    "ws://127.0.0.1:7789/ws?user=evan",
			  ],
			}
		`);
	});

	it("a refused hello as ourselves shows the sign-in form", async () => {
		const fake = new FakeBackend([session("A")]);
		localStorage.setItem("prigh-pi-web:as-user", "nobody");
		history.replaceState(null, "", "/?user=evan&token=bad");
		await start(fake);
		expect(localStorage.getItem("prigh-pi-web:as-user")).toBeNull();
		backend.push({ type: "prigh_hello_failed", error: "unauthorised" });
		await settle();
		expect(state.helloError.value).toBe("unauthorised");
		expect(backend.urls).toHaveLength(1);
	});

	it("signing out forgets it", async () => {
		localStorage.setItem("prigh-pi-web:user", "evan");
		localStorage.setItem("prigh-pi-web:as-user", "alice");
		const fake = new FakeBackend([session("A")]);
		fake.allowedAsUsers.add("alice");
		await start(fake);
		state.signOut();
		expect({ asUser: state.asUser.value, storage: { ...localStorage }, url: location.search }).toMatchInlineSnapshot(`
			{
			  "asUser": "",
			  "storage": {},
			  "url": "",
			}
		`);
	});
});
