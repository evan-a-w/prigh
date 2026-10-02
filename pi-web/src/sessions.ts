import type { PrighSession } from "./protocol.ts";

export function formatRelativeTime(iso: string | undefined, now = Date.now()): string | undefined {
	if (!iso) return undefined;
	const then = Date.parse(iso);
	if (Number.isNaN(then)) return undefined;
	const seconds = Math.max(0, Math.floor((now - then) / 1000));
	if (seconds < 60) return "just now";
	const minutes = Math.floor(seconds / 60);
	if (minutes < 60) return `${minutes}m ago`;
	const hours = Math.floor(minutes / 60);
	if (hours < 24) return `${hours}h ago`;
	const days = Math.floor(hours / 24);
	return `${days}d ago`;
}

/** The name, else the description, else the first prompt, else the id. */
export function sessionTitle(session: PrighSession): string {
	const raw = session.name ?? session.description ?? session.first_prompt ?? session.id;
	const line = raw.split("\n")[0].trim();
	return line.length > 80 ? `${line.slice(0, 77)}…` : line || session.id;
}
