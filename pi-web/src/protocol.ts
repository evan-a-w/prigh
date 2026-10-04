/**
 * Wire protocol types for the pi RPC protocol, one JSON message per WebSocket frame.
 *
 * Copied from pi's packages/web/src/protocol.ts. The prigh backend speaks this
 * protocol through its Pi_rpc adapter (backend/lib/pi_rpc.ml); the prigh-only
 * additions are marked below.
 */

// ============================================================================
// Content blocks & messages (packages/ai/src/types.ts)
// ============================================================================

export interface TextContent {
	type: "text";
	text: string;
	textSignature?: string;
}

export interface ThinkingContent {
	type: "thinking";
	thinking: string;
	thinkingSignature?: string;
	redacted?: boolean;
}

export interface ImageContent {
	type: "image";
	data: string; // base64
	mimeType: string;
}

export interface ToolCall {
	type: "toolCall";
	id: string;
	name: string;
	arguments: Record<string, unknown>;
}

export type StopReason = "stop" | "length" | "toolUse" | "error" | "aborted";

export interface UserMessage {
	role: "user";
	content: string | (TextContent | ImageContent)[];
	timestamp: number;
}

export interface AssistantMessage {
	role: "assistant";
	content: (TextContent | ThinkingContent | ToolCall)[];
	provider: string;
	model: string;
	stopReason: StopReason;
	errorMessage?: string;
	timestamp: number;
}

export interface ToolResultMessage {
	role: "toolResult";
	toolCallId: string;
	toolName: string;
	content: (TextContent | ImageContent)[];
	isError: boolean;
	timestamp: number;
}

// ============================================================================
// Custom agent messages (packages/coding-agent/src/core/messages.ts)
// ============================================================================

export interface BashExecutionMessage {
	role: "bashExecution";
	command: string;
	output: string;
	exitCode: number | undefined;
	cancelled: boolean;
	truncated: boolean;
	fullOutputPath?: string;
	timestamp: number;
}

export interface CustomMessage {
	role: "custom";
	customType: string;
	content: string | (TextContent | ImageContent)[];
	display: boolean;
	timestamp: number;
}

export interface BranchSummaryMessage {
	role: "branchSummary";
	summary: string;
	fromId: string;
	timestamp: number;
}

export interface CompactionSummaryMessage {
	role: "compactionSummary";
	summary: string;
	tokensBefore: number;
	timestamp: number;
}

export type AgentMessage =
	| UserMessage
	| AssistantMessage
	| ToolResultMessage
	| BashExecutionMessage
	| CustomMessage
	| BranchSummaryMessage
	| CompactionSummaryMessage;

// ============================================================================
// Session state & stats
// ============================================================================

export type ThinkingLevel = "off" | "minimal" | "low" | "medium" | "high" | "xhigh" | "max";

export interface Model {
	id: string;
	name: string;
	provider: string;
	reasoning: boolean;
	contextWindow: number;
}

export interface RpcSessionState {
	model?: Model;
	/** Working location the session currently runs in. */
	cwd: string;
	thinkingLevel: ThinkingLevel;
	isStreaming: boolean;
	isCompacting: boolean;
	steeringMode: "all" | "one-at-a-time";
	followUpMode: "all" | "one-at-a-time";
	sessionFile?: string;
	sessionId: string;
	sessionName?: string;
	autoCompactionEnabled: boolean;
	messageCount: number;
	pendingMessageCount: number;
}

export interface SessionStats {
	sessionFile: string | undefined;
	sessionId: string;
	userMessages: number;
	assistantMessages: number;
	toolCalls: number;
	toolResults: number;
	totalMessages: number;
	tokens: {
		input: number;
		output: number;
		cacheRead: number;
		cacheWrite: number;
		total: number;
	};
	cost: number;
	contextUsage?: {
		tokens: number | null;
		contextWindow: number;
		percent: number | null;
	};
}

export interface RpcSlashCommand {
	name: string;
	description?: string;
	source: "extension" | "prompt" | "skill" | "builtin";
	argumentHint?: string;
	sourceInfo?: Record<string, unknown>;
}

export interface BashResult {
	output: string;
	exitCode: number | undefined;
	cancelled: boolean;
	truncated: boolean;
	fullOutputPath?: string;
}

// ============================================================================
// RPC commands (client -> server)
// ============================================================================

export type RpcCommand =
	| { id?: string; type: "prompt"; message: string; images?: ImageContent[]; streamingBehavior?: "steer" | "followUp" }
	| { id?: string; type: "steer"; message: string; images?: ImageContent[] }
	| { id?: string; type: "follow_up"; message: string; images?: ImageContent[] }
	| { id?: string; type: "abort" }
	| { id?: string; type: "get_state" }
	| { id?: string; type: "get_messages" }
	| { id?: string; type: "get_commands" }
	| { id?: string; type: "get_session_stats" }
	| { id?: string; type: "set_model"; provider: string; modelId: string }
	| { id?: string; type: "set_thinking_level"; level: ThinkingLevel }
	| { id?: string; type: "cycle_thinking_level" }
	| { id?: string; type: "get_available_thinking_levels" }
	| { id?: string; type: "compact"; customInstructions?: string }
	| { id?: string; type: "set_session_name"; name: string }
	| { id?: string; type: "new_session"; parentSession?: string }
	| { id?: string; type: "get_available_models" }
	| { id?: string; type: "bash"; command: string; excludeFromContext?: boolean }
	| { id?: string; type: "export_html"; outputPath?: string }
	| { id?: string; type: "get_last_assistant_text" }
	| { id?: string; type: "get_fork_messages" }
	| { id?: string; type: "fork"; entryId: string }
	| { id?: string; type: "clone" }
	| { id?: string; type: "change_cwd"; cwd: string }
	// prigh additions: the backend's saved sessions, and switching this
	// connection to one of them (the client re-syncs afterwards).
	| { id?: string; type: "list_sessions" }
	| { id?: string; type: "switch_session"; path: string }
	// prigh addition: a subagent's conversation (by agent id or by the id of
	// the tool call that started it); its events then arrive as
	// prigh_subagent_event until the next watch_subagent (without ids: stop)
	// or a session change.
	| { id?: string; type: "watch_subagent"; agentId?: string; toolCallId?: string };

/** One node of the agents rail plus what the subagent view shows (watch_subagent). */
export interface SubagentInfo {
	id: string;
	label: string;
	state: "running" | "complete" | "failed";
	startedAt?: number;
	updatedAt?: number;
	endedAt?: number;
	activity?: { turnCount?: number; toolCount?: number; currentTool?: string; currentToolStartedAt?: number };
	task: string;
	model: string;
	callId: string;
	parentId: string | null;
	result: { text: string; isError: boolean } | null;
}

export interface SubagentTranscript {
	subagent: SubagentInfo;
	messages: AgentMessage[];
}

/** The watched subagent's own conversation events, or a change of its info. */
export type SubagentEvent =
	| { type: "message_start" | "message_update" | "message_end"; message: AgentMessage }
	| { type: "tool_execution_start"; toolCallId: string; toolName: string; args: Record<string, unknown> }
	| { type: "tool_execution_update"; toolCallId: string; toolName: string; partialResult: ToolResultLike }
	| { type: "tool_execution_end"; toolCallId: string; toolName: string; result: ToolResultLike; isError: boolean }
	| { type: "subagent_info"; subagent: SubagentInfo };

/** A saved prigh session (the backend's list_sessions entry). */
export interface PrighSession {
	id: string;
	path: string;
	name: string | null;
	description: string | null;
	cwd: string;
	created_at: string;
	updated_at: string;
	first_prompt: string | null;
	message_count: number;
	parent: string | null;
	live: boolean;
	running?: boolean;
	clients?: number;
}

export type RpcResponse =
	| { id?: string; type: "response"; command: "get_state"; success: true; data: RpcSessionState }
	| { id?: string; type: "response"; command: "get_messages"; success: true; data: { messages: AgentMessage[] } }
	| { id?: string; type: "response"; command: "get_commands"; success: true; data: { commands: RpcSlashCommand[] } }
	| { id?: string; type: "response"; command: "get_session_stats"; success: true; data: SessionStats }
	| { id?: string; type: "response"; command: string; success: true; data?: unknown }
	| { id?: string; type: "response"; command: string; success: false; error: string };

// ============================================================================
// Events (server -> client)
// ============================================================================

/** Streaming delta carried by message_update; the accumulated message is what matters. */
export interface AssistantMessageEvent {
	type: string;
}

export interface ToolResultLike {
	content?: unknown;
}

export type AgentSessionEvent =
	| { type: "agent_start" }
	| { type: "agent_end"; messages: AgentMessage[]; willRetry: boolean }
	| { type: "agent_settled" }
	| { type: "turn_start" }
	| { type: "turn_end"; message: AgentMessage; toolResults: ToolResultMessage[] }
	| { type: "message_start"; message: AgentMessage }
	| { type: "message_update"; message: AgentMessage; assistantMessageEvent: AssistantMessageEvent }
	| { type: "message_end"; message: AgentMessage }
	| { type: "bash_execution_update"; id?: string; delta: string }
	| { type: "session_reloaded" }
	| { type: "tool_execution_start"; toolCallId: string; toolName: string; args: Record<string, unknown> }
	| {
			type: "tool_execution_update";
			toolCallId: string;
			toolName: string;
			args: Record<string, unknown>;
			partialResult: ToolResultLike;
	  }
	| { type: "tool_execution_end"; toolCallId: string; toolName: string; result: ToolResultLike; isError: boolean }
	| { type: "queue_update"; steering: readonly string[]; followUp: readonly string[] }
	| { type: "compaction_start"; reason: "manual" | "threshold" | "overflow" }
	| {
			type: "compaction_end";
			reason: "manual" | "threshold" | "overflow";
			result?: { summary: string; tokensBefore: number };
			aborted: boolean;
			willRetry: boolean;
			errorMessage?: string;
	  }
	| { type: "auto_retry_start"; attempt: number; maxAttempts: number; delayMs: number; errorMessage: string }
	| { type: "auto_retry_end"; success: boolean; attempt: number; finalError?: string }
	| {
			type: "summarization_retry_scheduled";
			attempt: number;
			maxAttempts: number;
			delayMs: number;
			errorMessage: string;
	  }
	| { type: "summarization_retry_attempt_start"; source: "compaction" | "branchSummary"; reason?: string }
	| { type: "summarization_retry_finished" }
	| { type: "session_info_changed"; name: string | undefined }
	| { type: "thinking_level_changed"; level: ThinkingLevel }
	| { type: "extension_error"; extensionPath: string; event: string; error: string }
	| { type: "extension_event"; channel: string; data: unknown }
	// prigh addition: the backend refused this connection's hello (bad token,
	// unknown session); the socket is closed right after.
	| { type: "prigh_hello_failed"; error: string }
	| { type: "prigh_subagent_event"; agentId: string; event: SubagentEvent }
	// prigh addition: /setusr succeeded; reconnect acting as this user.
	| { type: "prigh_set_user"; user: string };

// ============================================================================
// Extension UI sub-protocol
// ============================================================================

export type RpcExtensionUIRequest =
	| { type: "extension_ui_request"; id: string; method: "select"; title: string; options: string[]; timeout?: number }
	| { type: "extension_ui_request"; id: string; method: "confirm"; title: string; message: string; timeout?: number }
	| {
			type: "extension_ui_request";
			id: string;
			method: "input";
			title: string;
			placeholder?: string;
			timeout?: number;
	  }
	| { type: "extension_ui_request"; id: string; method: "editor"; title: string; prefill?: string }
	| {
			type: "extension_ui_request";
			id: string;
			method: "notify";
			message: string;
			notifyType?: "info" | "warning" | "error";
	  }
	| {
			type: "extension_ui_request";
			id: string;
			method: "setStatus";
			statusKey: string;
			statusText: string | undefined;
	  }
	| {
			type: "extension_ui_request";
			id: string;
			method: "setWidget";
			widgetKey: string;
			widgetLines: string[] | undefined;
			widgetPlacement?: "aboveEditor" | "belowEditor";
	  }
	| { type: "extension_ui_request"; id: string; method: "setTitle"; title: string }
	| { type: "extension_ui_request"; id: string; method: "set_editor_text"; text: string };

/**
 * Sent to all clients except the one that answered a dialog, and to all
 * clients when a dialog times out or is aborted. Dismiss the dialog with
 * this id, if shown.
 */
export interface RpcExtensionUICancel {
	type: "extension_ui_cancel";
	id: string;
}

export type RpcExtensionUIResponse =
	| { type: "extension_ui_response"; id: string; value: string }
	| { type: "extension_ui_response"; id: string; confirmed: boolean }
	| { type: "extension_ui_response"; id: string; cancelled: true };
