/**
 * The subagent view's state: which subagent is open and its conversation,
 * kept live from `prigh_subagent_event`s. Pure; state.ts holds the signal
 * and talks to the backend (watch_subagent).
 */
import type { AgentMessage, SubagentEvent, SubagentInfo, SubagentTranscript } from "./protocol.ts";
import type { AsyncStatusSnapshot, AsyncStatusSnapshotNode } from "./subagent-status.ts";
import { applyToolEvent, rebuildToolStates, type ToolStates, upsertMessage } from "./transcript.ts";

export type SubagentTarget = { agentId: string } | { toolCallId: string };

export interface SubagentView {
	target: SubagentTarget;
	/** Known once the backend answered (a target by tool call id has none before). */
	agentId: string | undefined;
	status: "loading" | "ready" | "error";
	error?: string;
	info?: SubagentInfo;
	messages: AgentMessage[];
	toolStates: ToolStates;
	/** Events that arrived before the transcript, replayed onto it. */
	pending: Array<{ agentId: string; event: SubagentEvent }>;
}

export function openView(target: SubagentTarget): SubagentView {
	return {
		target,
		agentId: "agentId" in target ? target.agentId : undefined,
		status: "loading",
		messages: [],
		toolStates: {},
		pending: [],
	};
}

export function sameTarget(view: SubagentView | undefined, target: SubagentTarget): boolean {
	if (!view) return false;
	if ("agentId" in target) return view.agentId === target.agentId;
	return "toolCallId" in view.target
		? view.target.toolCallId === target.toolCallId
		: view.info?.callId === target.toolCallId;
}

export function applyEvent(view: SubagentView, event: SubagentEvent): SubagentView {
	switch (event.type) {
		case "message_start":
		case "message_update":
		case "message_end":
			return { ...view, messages: upsertMessage(view.messages, event.message) };
		case "subagent_info":
			return { ...view, info: event.subagent };
		default:
			return { ...view, toolStates: applyToolEvent(view.toolStates, event) };
	}
}

/** A `prigh_subagent_event`: applied when it is the open subagent's, kept until the transcript arrives. */
export function receive(view: SubagentView, agentId: string, event: SubagentEvent): SubagentView {
	if (view.status === "loading") return { ...view, pending: [...view.pending, { agentId, event }] };
	if (view.status === "ready" && agentId === view.agentId) return applyEvent(view, event);
	return view;
}

/** The watch_subagent answer. Replaying the early events is safe: messages are upserted by timestamp. */
export function loaded(view: SubagentView, transcript: SubagentTranscript): SubagentView {
	const agentId = transcript.subagent.id;
	let next: SubagentView = {
		...view,
		agentId,
		status: "ready",
		error: undefined,
		info: transcript.subagent,
		messages: transcript.messages,
		toolStates: rebuildToolStates(transcript.messages),
		pending: [],
	};
	for (const pending of view.pending) {
		if (pending.agentId === agentId) next = applyEvent(next, pending.event);
	}
	return next;
}

export function failed(view: SubagentView, error: string): SubagentView {
	return { ...view, status: "error", error, pending: [] };
}

export interface ListedNode {
	node: AsyncStatusSnapshotNode;
	depth: number;
}

/** The agents rail's tree, flattened in display order. */
export function listNodes(snapshot: AsyncStatusSnapshot | undefined): ListedNode[] {
	const listed: ListedNode[] = [];
	const walk = (nodes: AsyncStatusSnapshotNode[], depth: number) => {
		for (const node of nodes) {
			listed.push({ node, depth });
			if (node.children) walk(node.children, depth + 1);
		}
	};
	walk(snapshot?.runs ?? [], 0);
	return listed;
}
