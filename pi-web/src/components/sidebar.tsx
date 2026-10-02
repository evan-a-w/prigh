import { useEffect } from "preact/hooks";
import { formatRelativeTime, sessionTitle } from "../sessions.ts";
import { newSession, refreshSessions, sessionState, sessions, sidebarOpen, switchSession } from "../state.ts";

/**
 * Persistent left sidebar: app title + new-session button, the current
 * working directory, and the backend's saved sessions (most recent first;
 * clicking one moves this connection to it). Inline on desktop (>=900px);
 * below that (see .sidebar in style.css) it becomes an off-canvas drawer
 * toggled from the topbar.
 */
export function Sidebar() {
	useEffect(() => {
		void refreshSessions();
		const timer = setInterval(() => void refreshSessions(), 30_000);
		return () => clearInterval(timer);
	}, []);

	const isOpen = sidebarOpen.value;
	const state = sessionState.value;
	const cwd = state?.cwd;
	const currentId = state?.sessionId;

	return (
		<>
			{isOpen ? (
				<button
					type="button"
					class="sidebar-backdrop"
					aria-label="Close sidebar"
					onClick={() => {
						sidebarOpen.value = false;
					}}
				/>
			) : null}
			<nav class={`sidebar${isOpen ? " open" : ""}`} aria-label="Sessions">
				<div class="sidebar-header">
					<span class="sidebar-title">prigh</span>
					<button
						type="button"
						class="sidebar-new-btn"
						title="Start a new session"
						onClick={() => void newSession()}
					>
						<svg width="12" height="12" viewBox="0 0 16 16" fill="none" aria-hidden="true">
							<title>New session</title>
							<path d="M8 2v12M2 8h12" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" />
						</svg>
						New
					</button>
					<button
						type="button"
						class="sidebar-close-btn"
						title="Close sidebar"
						onClick={() => {
							sidebarOpen.value = false;
						}}
					>
						<svg width="14" height="14" viewBox="0 0 16 16" fill="none" aria-hidden="true">
							<title>Close</title>
							<path d="M3 3l10 10M13 3L3 13" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" />
						</svg>
					</button>
				</div>
				{cwd ? (
					<div class="sidebar-project" title={cwd}>
						<span dir="ltr">{cwd}</span>
					</div>
				) : null}
				{sessions.value.length > 0 ? (
					<div class="sidebar-sessions">
						<div class="sidebar-section-label">Sessions</div>
						{sessions.value.map((session) => (
							<button
								key={session.id}
								type="button"
								class={`session-item${session.id === currentId ? " current" : ""}`}
								title={`${sessionTitle(session)}\n${session.path}`}
								onClick={() => void switchSession(session.path)}
							>
								<div class="session-item-title">
									<span class="session-item-name">{sessionTitle(session)}</span>
									{session.running ? <span class="session-item-ns">running</span> : null}
								</div>
								<div class="session-item-meta">
									{[formatRelativeTime(session.updated_at), `${session.message_count} msgs`].join(" · ")}
								</div>
							</button>
						))}
					</div>
				) : null}
			</nav>
		</>
	);
}
