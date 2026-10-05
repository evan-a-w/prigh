import { useEffect, useState } from "preact/hooks";
import type { SubagentInfo } from "../protocol.ts";
import { closeSubagent, openSubagent, subagentSnapshot, subagentView } from "../state.ts";
import type { AsyncStatusSnapshotState } from "../subagent-status.ts";
import { listNodes } from "../subagent-view.ts";
import { Conversation } from "./chat-list.tsx";
import { ToolStatesContext } from "./tool-execution.tsx";

/** The status classes style.css has (from pi's run history). */
function statusClass(state: AsyncStatusSnapshotState | SubagentInfo["state"]): string {
	if (state === "complete") return "done";
	if (state === "failed" || state === "stopped" || state === "rejected") return "failed";
	return "running";
}

function formatTime(ms: number | undefined): string {
	if (ms === undefined || !Number.isFinite(ms)) return "—";
	const date = new Date(ms);
	const pad = (value: number): string => String(value).padStart(2, "0");
	return `${date.getHours()}:${pad(date.getMinutes())}:${pad(date.getSeconds())}`;
}

function formatDuration(ms: number | undefined): string {
	if (ms === undefined || !Number.isFinite(ms)) return "—";
	const seconds = Math.max(0, Math.floor(ms / 1000));
	if (seconds < 60) return `${seconds}s`;
	const minutes = Math.floor(seconds / 60);
	if (minutes < 60) return `${minutes}m ${seconds % 60}s`;
	return `${Math.floor(minutes / 60)}h ${minutes % 60}m`;
}

function duration(info: SubagentInfo, now: number): number | undefined {
	if (info.startedAt === undefined) return undefined;
	return (info.endedAt ?? (info.state === "running" ? now : (info.updatedAt ?? now))) - info.startedAt;
}

function SubagentMeta({ info, now }: { info: SubagentInfo; now: number }) {
	const activity = info.activity ?? {};
	return (
		<div class="subagents-meta">
			<div class="subagents-meta-line">
				<span class={`status-dot ${statusClass(info.state)}`} />
				<span class="subagents-meta-agent">{info.label}</span>
				<span class="subagents-meta-muted">{info.id}</span>
				<span class={`status-label ${statusClass(info.state)}`}>{info.state}</span>
			</div>
			<div class="subagents-meta-grid">
				<span>Started</span>
				<strong>{formatTime(info.startedAt)}</strong>
				<span>Duration</span>
				<strong>{formatDuration(duration(info, now))}</strong>
				<span>Model</span>
				<strong>{info.model || "—"}</strong>
				<span>Turns · tools</span>
				<strong>
					{activity.turnCount ?? 0} · {activity.toolCount ?? 0}
				</strong>
				{activity.currentTool ? (
					<>
						<span>Running</span>
						<strong>{activity.currentTool}</strong>
					</>
				) : null}
			</div>
			{info.result?.isError ? <div class="subagents-error">{info.result.text}</div> : null}
		</div>
	);
}

/**
 * A subagent's conversation in place of the chat, opened from the agents
 * rail or a `subagent` tool call; live while the subagent runs (see
 * state.ts openSubagent). The left list is the agents rail's tree, to
 * switch between subagents.
 */
export function SubagentPanel() {
	const view = subagentView.value;
	const [now, setNow] = useState(() => Date.now());
	const running = view?.info?.state === "running";
	useEffect(() => {
		if (!running) return;
		const timer = setInterval(() => setNow(Date.now()), 1000);
		return () => clearInterval(timer);
	}, [running]);
	if (!view) return null;

	const listed = listNodes(subagentSnapshot.value);
	const info = view.info;
	const missing = info && !listed.some(({ node }) => node.id === info.id);

	return (
		<div class="subagents-panel subagent-view">
			<div class="subagents-panel-header">
				<span>Subagent{info ? ` ${info.id}` : ""}</span>
				<button type="button" class="subagents-back-to-chat" onClick={closeSubagent}>
					← Back to chat
				</button>
			</div>
			<div class="subagents-layout">
				<div class="subagents-list">
					{missing && info ? (
						<button type="button" class="subagents-list-item active" title={info.task}>
							<div class="subagents-list-item-row">
								<span class={`status-dot ${statusClass(info.state)}`} />
								<span class="subagents-list-item-agent">{info.label}</span>
								<span class={`subagents-list-status ${statusClass(info.state)}`}>{info.state}</span>
							</div>
						</button>
					) : null}
					{listed.map(({ node, depth }) => (
						<button
							type="button"
							key={node.id}
							class={`subagents-list-item ${node.id === view.agentId ? "active" : ""}`}
							style={depth > 0 ? { paddingLeft: `${10 + depth * 14}px` } : undefined}
							title={node.label}
							onClick={() => void openSubagent({ agentId: node.id })}
						>
							<div class="subagents-list-item-row">
								<span class={`status-dot ${statusClass(node.state)}`} />
								<span class="subagents-list-item-agent">{node.label}</span>
								<span class={`subagents-list-status ${statusClass(node.state)}`}>{node.state}</span>
							</div>
							<div class="subagents-list-item-row">
								<span class="subagents-list-item-runid">{node.id}</span>
								<span class="subagents-list-item-time">{formatTime(node.startedAt)}</span>
							</div>
						</button>
					))}
				</div>
				<div class="subagents-detail">
					{info ? <SubagentMeta info={info} now={now} /> : null}
					{view.status === "loading" ? <div class="subagents-loading">Loading…</div> : null}
					{view.status === "error" ? (
						<div class="subagents-empty">
							This subagent's conversation is not available ({view.error}). The backend keeps it in memory only
							while the session is loaded.
						</div>
					) : null}
					{view.status === "ready" ? (
						<ToolStatesContext.Provider value={() => subagentView.value?.toolStates ?? {}}>
							<Conversation
								history={view.messages}
								emptyText={info?.state === "running" ? "Starting…" : "No messages."}
							/>
						</ToolStatesContext.Provider>
					) : null}
				</div>
			</div>
		</div>
	);
}
