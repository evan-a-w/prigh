// Slash command parsing and completion.

export interface CommandSpec {
	name: string;
	args: string;
	help: string;
}

export const COMMANDS: readonly CommandSpec[] = [
	{ name: "help", args: "", help: "show this help" },
	{ name: "model", args: "[id]", help: "show or switch the model" },
	{ name: "thinking", args: "[off|on|low|high|max]", help: "show or set the thinking level" },
	{ name: "login", args: "[provider] [api_key|oauth]", help: "log in to a provider (anthropic, openai, openai-codex, deepseek)" },
	{ name: "logout", args: "<provider>", help: "remove a provider's stored credential" },
	{ name: "auth", args: "", help: "show which providers are configured" },
	{ name: "compact", args: "", help: "summarise older messages to free context" },
	{ name: "new", args: "", help: "start a new session" },
	{ name: "sessions", args: "", help: "list saved sessions" },
	{ name: "switch", args: "<number|path>", help: "switch to a saved session (number from /sessions)" },
	{ name: "fork", args: "", help: "fork the current session" },
	{ name: "abort", args: "", help: "abort the current run" },
	{ name: "state", args: "", help: "show session state" },
	{ name: "clear", args: "", help: "clear the screen" },
	{ name: "quit", args: "", help: "exit" },
];

export interface ParsedCommand {
	name: string;
	args: string[];
	rest: string;
}

export function parseCommand(input: string): ParsedCommand | null {
	const trimmed = input.trim();
	if (!trimmed.startsWith("/")) return null;
	const [head, ...tail] = trimmed.slice(1).split(/\s+/);
	if (!head) return null;
	return { name: head, args: tail.filter((a) => a !== ""), rest: trimmed.slice(1 + head.length).trim() };
}

/** Completes a partial "/cmd" prefix; returns the unique completion or null. */
export function completeCommand(input: string): string | null {
	if (!input.startsWith("/") || input.includes(" ")) return null;
	const prefix = input.slice(1);
	const matches = COMMANDS.filter((c) => c.name.startsWith(prefix));
	if (matches.length === 1) return `/${matches[0]?.name} `;
	if (matches.length === 0) return null;
	let common = matches[0]?.name ?? "";
	for (const m of matches) {
		while (!m.name.startsWith(common)) common = common.slice(0, -1);
	}
	return common.length > prefix.length ? `/${common}` : null;
}

export function helpText(): string {
	const width = Math.max(...COMMANDS.map((c) => `/${c.name} ${c.args}`.length));
	return COMMANDS.map((c) => `${`/${c.name} ${c.args}`.padEnd(width)}  ${c.help}`).join("\n");
}
