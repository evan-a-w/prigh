import { describe, expect, it } from "vitest";
import type { AgentMessage, SubagentInfo } from "../src/protocol.ts";
import { applyEvent, failed, listNodes, loaded, openView, receive, sameTarget } from "../src/subagent-view.ts";
import type { AsyncStatusSnapshotNode } from "../src/subagent-status.ts";
import { applyToolEvent, rebuildToolStates, upsertMessage } from "../src/transcript.ts";

const info = (id: string, callId = `call-${id}`): SubagentInfo => ({
	id,
	label: id,
	state: "running",
	task: id,
	model: "faux",
	callId,
	parentId: null,
	result: null,
});

const user = (content: string, timestamp: number): AgentMessage => ({ role: "user", content, timestamp });
const assistant = (text: string, timestamp: number): AgentMessage => ({
	role: "assistant",
	content: [{ type: "text", text }],
	provider: "prigh",
	model: "faux",
	stopReason: "stop",
	timestamp,
});

const summary = (messages: AgentMessage[]) =>
	messages.map(
		(m) =>
			`${m.role}@${m.timestamp}:${"content" in m && typeof m.content !== "string" ? m.content.map((c) => ("text" in c ? c.text : c.type)).join("") : "content" in m ? m.content : ""}`,
	);

describe("transcript reducers", () => {
	it("upserts by role and timestamp", () => {
		let list = [user("task", 0)];
		list = upsertMessage(list, assistant("Hel", 1));
		list = upsertMessage(list, assistant("Hello", 1));
		list = upsertMessage(list, user("steer", 1));
		expect(summary(list)).toEqual(["user@0:task", "assistant@1:Hello", "user@1:steer"]);
	});

	it("tracks tool calls from events, and from the history alone", () => {
		let states = applyToolEvent(
			{},
			{ type: "tool_execution_start", toolCallId: "t1", toolName: "bash", args: { command: "ls" } },
		);
		states = applyToolEvent(states, {
			type: "tool_execution_update",
			toolCallId: "t1",
			partialResult: { content: [{ type: "text", text: "a" }] },
		});
		expect(states.t1).toEqual({
			name: "bash",
			args: { command: "ls" },
			status: "running",
			partial: "a",
			output: undefined,
			isError: undefined,
		});
		states = applyToolEvent(states, {
			type: "tool_execution_end",
			toolCallId: "t1",
			result: { content: [{ type: "text", text: "a\nb" }] },
			isError: true,
		});
		expect(states.t1).toMatchObject({ status: "done", output: "a\nb", isError: true });

		expect(
			rebuildToolStates([
				{
					role: "assistant",
					content: [
						{ type: "toolCall", id: "x", name: "ls", arguments: {} },
						{ type: "toolCall", id: "y", name: "read", arguments: { path: "f" } },
					],
					provider: "p",
					model: "m",
					stopReason: "toolUse",
					timestamp: 1,
				},
				{
					role: "toolResult",
					toolCallId: "x",
					toolName: "ls",
					content: [{ type: "text", text: "f" }],
					isError: false,
					timestamp: 2,
				},
			]),
		).toEqual({
			x: { name: "ls", args: {}, status: "done", output: "f", isError: false },
			y: { name: "read", args: { path: "f" }, status: "running", output: undefined, isError: undefined },
		});
	});
});

describe("subagent view state", () => {
	it("keeps events until the transcript arrives, then replays the open subagent's", () => {
		let view = openView({ toolCallId: "call-a1" });
		expect(view.agentId).toBeUndefined();
		view = receive(view, "a1", { type: "message_update", message: assistant("Wor", 2) });
		view = receive(view, "a2", { type: "message_end", message: user("other agent", 5) });
		view = receive(view, "a1", { type: "message_update", message: assistant("Working", 2) });
		view = loaded(view, { subagent: info("a1"), messages: [user("task", 0), assistant("Looking", 1)] });
		expect({
			agentId: view.agentId,
			status: view.status,
			pending: view.pending,
			messages: summary(view.messages),
		}).toEqual({
			agentId: "a1",
			status: "ready",
			pending: [],
			messages: ["user@0:task", "assistant@1:Looking", "assistant@2:Working"],
		});
		view = receive(view, "a2", { type: "message_end", message: user("other agent", 3) });
		view = receive(view, "a1", { type: "subagent_info", subagent: { ...info("a1"), state: "complete" } });
		expect(summary(view.messages)).toHaveLength(3);
		expect(view.info?.state).toBe("complete");
	});

	it("matches targets by agent id or starting tool call", () => {
		const byCall = loaded(openView({ toolCallId: "c9" }), { subagent: info("a3", "c9"), messages: [] });
		expect([
			sameTarget(byCall, { agentId: "a3" }),
			sameTarget(byCall, { toolCallId: "c9" }),
			sameTarget(openView({ agentId: "a3" }), { toolCallId: "c9" }),
			sameTarget(loaded(openView({ agentId: "a3" }), { subagent: info("a3", "c9"), messages: [] }), {
				toolCallId: "c9",
			}),
			sameTarget(undefined, { agentId: "a3" }),
		]).toEqual([true, true, false, true, false]);
	});

	it("fails without dropping the target, and ignores events then", () => {
		let view = failed(openView({ agentId: "a1" }), "unknown subagent");
		view = receive(view, "a1", { type: "message_end", message: user("late", 0) });
		expect(view).toMatchObject({ status: "error", error: "unknown subagent", messages: [], agentId: "a1" });
		expect(applyEvent(view, { type: "message_end", message: user("x", 0) }).messages).toHaveLength(1);
	});

	it("lists the rail's tree depth first", () => {
		const node = (id: string, children?: AsyncStatusSnapshotNode[]): AsyncStatusSnapshotNode => ({
			id,
			kind: "subagent" as const,
			label: id,
			state: "running" as const,
			children,
		});
		expect(
			listNodes({
				generatedAt: 0,
				omitted: { runs: 0, children: 0, byteLimitExceeded: false },
				runs: [node("a1", [node("a1/c1", [node("a1/c1/c2")])]), node("a2")],
			}).map(({ node, depth }) => `${"  ".repeat(depth)}${node.id}`),
		).toEqual(["a1", "  a1/c1", "    a1/c1/c2", "a2"]);
		expect(listNodes(undefined)).toEqual([]);
	});
});
