/**
 * Pure reducers over a conversation as the chat shows it: the message list
 * (pi keys messages by role + timestamp) and the live state of its tool
 * calls. Used for the session's chat (state.ts) and a subagent's
 * (subagent-view.ts).
 */
import type { AgentMessage, ToolResultLike, ToolResultMessage } from "./protocol.ts";

export interface ToolDisplayState {
	name: string;
	args: Record<string, unknown>;
	status: "running" | "done";
	partial?: string;
	output?: string;
	isError?: boolean;
}

export type ToolStates = Record<string, ToolDisplayState>;

export function upsertMessage(list: AgentMessage[], message: AgentMessage): AgentMessage[] {
	for (let i = list.length - 1; i >= 0; i--) {
		const existing = list[i];
		if (existing.role === message.role && existing.timestamp === message.timestamp) {
			const next = [...list];
			next[i] = message;
			return next;
		}
	}
	return [...list, message];
}

export function setToolState(states: ToolStates, toolCallId: string, updates: Partial<ToolDisplayState>): ToolStates {
	const existing = states[toolCallId];
	return {
		...states,
		[toolCallId]: {
			name: updates.name ?? existing?.name ?? "tool",
			args: updates.args ?? existing?.args ?? {},
			status: updates.status ?? existing?.status ?? "running",
			...updates,
		},
	};
}

export function extractResultText(result: ToolResultLike | undefined): string | undefined {
	if (!result || !Array.isArray(result.content)) {
		return undefined;
	}
	const texts: string[] = [];
	for (const block of result.content) {
		if (block && typeof block === "object" && "text" in block && typeof block.text === "string") {
			texts.push(block.text);
		}
	}
	return texts.length > 0 ? texts.join("\n") : undefined;
}

/** Tool states as the history alone tells them: calls without a result are still running. */
export function rebuildToolStates(history: AgentMessage[]): ToolStates {
	const resultsByToolCallId = new Map<string, ToolResultMessage>();
	for (const message of history) {
		if (message.role === "toolResult") {
			resultsByToolCallId.set(message.toolCallId, message);
		}
	}
	const states: ToolStates = {};
	for (const message of history) {
		if (message.role !== "assistant") continue;
		for (const toolCall of message.content) {
			if (toolCall.type !== "toolCall") continue;
			const result = resultsByToolCallId.get(toolCall.id);
			states[toolCall.id] = {
				name: toolCall.name,
				args: toolCall.arguments,
				status: result ? "done" : "running",
				output: result ? extractResultText(result) : undefined,
				isError: result?.isError,
			};
		}
	}
	return states;
}

export type ConversationEvent =
	| { type: "message_start" | "message_update" | "message_end"; message: AgentMessage }
	| { type: "tool_execution_start"; toolCallId: string; toolName: string; args?: Record<string, unknown> }
	| { type: "tool_execution_update"; toolCallId: string; partialResult?: ToolResultLike }
	| { type: "tool_execution_end"; toolCallId: string; result?: ToolResultLike; isError?: boolean };

export function applyToolEvent(states: ToolStates, event: ConversationEvent): ToolStates {
	switch (event.type) {
		case "tool_execution_start":
			return setToolState(states, event.toolCallId, {
				name: event.toolName,
				args: event.args ?? {},
				status: "running",
				partial: undefined,
				output: undefined,
				isError: undefined,
			});
		case "tool_execution_update":
			return setToolState(states, event.toolCallId, { partial: extractResultText(event.partialResult) });
		case "tool_execution_end":
			return setToolState(states, event.toolCallId, {
				status: "done",
				output: extractResultText(event.result),
				isError: event.isError,
			});
		default:
			return states;
	}
}
