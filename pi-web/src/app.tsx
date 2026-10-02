import { useState } from "preact/hooks";
import { AgentsRail } from "./components/agents-rail.tsx";
import { ChatList } from "./components/chat-list.tsx";
import { DialogHost, ToastHost } from "./components/dialogs.tsx";
import { Editor } from "./components/editor.tsx";
import { StatusStrip } from "./components/footer.tsx";
import { MarkdownView } from "./components/markdown-view.tsx";
import { ForkPicker, ModelPicker } from "./components/pickers.tsx";
import { Sidebar } from "./components/sidebar.tsx";
import { saveToken } from "./connection.ts";
import {
	agentsRailOpen,
	commandResult,
	connected,
	helloError,
	sidebarOpen,
	stats,
	subagentSnapshot,
	toggleAgentsRail,
	widgets,
} from "./state.ts";
import { countRunningNodes } from "./subagent-status.ts";
import { applyTheme, themeName } from "./theme.ts";

/** At most 3 significant digits: 241k, 1.2M, 12.3k, 999. */
function formatCompactNumber(value: number): string {
	const abs = Math.abs(value);
	if (abs < 1000) return String(Math.round(value));
	const units: Array<[number, string]> = [
		[1_000_000_000, "B"],
		[1_000_000, "M"],
		[1_000, "k"],
	];
	for (const [threshold, suffix] of units) {
		if (abs >= threshold) {
			const scaled = value / threshold;
			const intDigits = Math.floor(Math.log10(Math.abs(scaled))) + 1;
			const decimals = Math.max(0, Math.min(2, 3 - intDigits));
			return `${scaled.toFixed(decimals)}${suffix}`;
		}
	}
	return String(Math.round(value));
}

function ThemeToggle() {
	const isLight = /light/i.test(themeName.value);
	return (
		<button
			type="button"
			class="topbar-icon-btn"
			title={isLight ? "Switch to dark theme" : "Switch to light theme"}
			onClick={() => void applyTheme(isLight ? "dark" : "light")}
		>
			{isLight ? (
				<svg width="15" height="15" viewBox="0 0 16 16" fill="none" aria-hidden="true">
					<title>Dark theme</title>
					<path
						d="M13.5 9.5A5.5 5.5 0 016.5 2.5 5.5 5.5 0 1013.5 9.5z"
						stroke="currentColor"
						stroke-width="1.2"
						fill="none"
					/>
				</svg>
			) : (
				<svg width="15" height="15" viewBox="0 0 16 16" fill="none" aria-hidden="true">
					<title>Light theme</title>
					<circle cx="8" cy="8" r="3.2" stroke="currentColor" stroke-width="1.2" />
					<path
						d="M8 1v2M8 13v2M1 8h2M13 8h2M3 3l1.4 1.4M11.6 11.6L13 13M13 3l-1.4 1.4M4.4 11.6L3 13"
						stroke="currentColor"
						stroke-width="1.2"
						stroke-linecap="round"
					/>
				</svg>
			)}
		</button>
	);
}

function UsageStats() {
	const sessionStats = stats.value;
	if (!sessionStats) return null;
	const context = sessionStats.contextUsage;
	const hasContext = context && context.percent !== null && context.percent !== undefined;
	const inOutTitle =
		sessionStats.tokens.cacheRead > 0
			? `${formatCompactNumber(sessionStats.tokens.input)} in · ${formatCompactNumber(sessionStats.tokens.output)} out · ${formatCompactNumber(sessionStats.tokens.cacheRead)} cache read`
			: `${formatCompactNumber(sessionStats.tokens.input)} in · ${formatCompactNumber(sessionStats.tokens.output)} out`;
	return (
		<span class="topbar-usage">
			<span title="Session cost">${sessionStats.cost.toFixed(2)}</span>
			{hasContext && context && (
				<span
					class="topbar-usage-ctx"
					title={`${context.tokens !== null ? formatCompactNumber(context.tokens) : "?"} / ${formatCompactNumber(context.contextWindow)} tokens`}
				>
					{Math.round(context.percent ?? 0)}% ctx
				</span>
			)}
			<span class="topbar-usage-tokens" title={inOutTitle}>
				{formatCompactNumber(sessionStats.tokens.input)} in · {formatCompactNumber(sessionStats.tokens.output)} out
			</span>
		</span>
	);
}

function TopBar() {
	const isConnected = connected.value;

	return (
		<header class="topbar">
			<div class="topbar-left">
				<button
					type="button"
					class="topbar-icon-btn topbar-sidebar-toggle"
					title="Toggle sidebar"
					onClick={() => {
						sidebarOpen.value = !sidebarOpen.value;
					}}
				>
					<svg width="16" height="16" viewBox="0 0 16 16" fill="none" aria-hidden="true">
						<title>Toggle sidebar</title>
						<path
							d="M2 3.5h12M2 8h12M2 12.5h12"
							stroke="currentColor"
							stroke-width="1.3"
							stroke-linecap="round"
						/>
					</svg>
				</button>
				<ThemeToggle />
				<button
					type="button"
					class={`topbar-btn ${agentsRailOpen.value ? "active" : ""}`}
					title="Toggle live agents panel"
					onClick={toggleAgentsRail}
				>
					<svg width="14" height="14" viewBox="0 0 16 16" fill="none" aria-hidden="true">
						<title>Agents</title>
						<rect x="2" y="3" width="12" height="10" rx="1.5" stroke="currentColor" stroke-width="1.2" />
						<path d="M5 6.5h6M5 9.5h4" stroke="currentColor" stroke-width="1.2" stroke-linecap="round" />
					</svg>
					<span class="topbar-btn-label">Agents</span>
					{countRunningNodes(subagentSnapshot.value) > 0 ? (
						<span class="topbar-btn-count">{countRunningNodes(subagentSnapshot.value)}</span>
					) : null}
				</button>
			</div>
			<div class="topbar-right">
				<UsageStats />
				<span
					class={`connection-dot ${isConnected ? "online" : "offline"}`}
					title={isConnected ? "Connected" : "Disconnected"}
				/>
			</div>
		</header>
	);
}

function WidgetArea({ placement }: { placement: "aboveEditor" | "belowEditor" }) {
	const entries = Object.entries(widgets.value).filter(([, widget]) => widget.placement === placement);
	if (entries.length === 0) return null;
	return (
		<div class="widget-area">
			{entries.map(([key, widget]) => (
				<pre key={key} class="widget">
					{widget.lines.join("\n")}
				</pre>
			))}
		</div>
	);
}

function CommandResultCard() {
	const result = commandResult.value;
	if (!result) return null;
	return (
		<div class="command-result">
			<div class="command-result-header">
				<span class="command-result-title">{result.title}</span>
				<button
					type="button"
					class="command-result-close"
					title="Dismiss"
					onClick={() => {
						commandResult.value = undefined;
					}}
				>
					×
				</button>
			</div>
			<MarkdownView text={result.markdown} />
		</div>
	);
}

/**
 * Shown when the backend refused the connection: the token is saved and the
 * page reloads (the session id, if any, stays in the query string).
 */
function ConnectView({ error }: { error: string }) {
	const [token, setToken] = useState("");
	return (
		<div class="unreachable-view">
			<div class="unreachable-card">
				<h1>Connect to prigh</h1>
				<p class="connect-error">{error}</p>
				<form
					class="connect-form"
					onSubmit={(event) => {
						event.preventDefault();
						saveToken(localStorage, token.trim());
						location.reload();
					}}
				>
					<input
						type="password"
						placeholder="Token (prigh serve -token …)"
						value={token}
						autocomplete="off"
						onInput={(event) => setToken((event.target as HTMLInputElement).value)}
					/>
					<button type="submit" class="unreachable-home">
						Connect
					</button>
				</form>
			</div>
		</div>
	);
}

export function App() {
	if (helloError.value !== undefined) {
		return <ConnectView error={helloError.value} />;
	}
	return (
		<div class="app-shell">
			<Sidebar />
			<div class="main">
				<TopBar />
				<div class="main-content-row">
					<div class="main-content">
						<ChatList />
						<CommandResultCard />
						<WidgetArea placement="aboveEditor" />
						<Editor />
						<WidgetArea placement="belowEditor" />
					</div>
					<AgentsRail />
				</div>
				<StatusStrip />
				<DialogHost />
				<ToastHost />
				<ModelPicker />
				<ForkPicker />
			</div>
		</div>
	);
}
