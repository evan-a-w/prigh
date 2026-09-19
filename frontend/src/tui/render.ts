// Pure rendering of transcript items and the bottom panel to strings.

import { ansi, renderMarkdown } from "../markdown.js";
import type { Message, State, ToolCall, ToolResultMessage } from "../protocol.js";

export interface Style {
	color: boolean;
	width: number;
}

function paint(style: Style, code: string, text: string): string {
	return style.color ? `${code}${text}${ansi.reset}` : text;
}

export function renderUser(text: string, style: Style): string {
	return text
		.split("\n")
		.map((line) => paint(style, ansi.bold + ansi.green, "> ") + paint(style, ansi.bold, line))
		.join("\n");
}

export function renderThinking(text: string, style: Style): string {
	return text
		.split("\n")
		.map((line) => paint(style, ansi.gray, `  ${line}`))
		.join("\n");
}

export function renderAssistantText(text: string, style: Style): string {
	return renderMarkdown(text, style);
}

export function summariseArguments(call: ToolCall): string {
	try {
		const args = JSON.parse(call.arguments) as Record<string, unknown>;
		if (typeof args !== "object" || args === null) return call.arguments;
		const parts: string[] = [];
		for (const [key, value] of Object.entries(args)) {
			const shown = typeof value === "string" ? value : JSON.stringify(value);
			const oneLine = shown.replace(/\n/g, "⏎");
			parts.push(`${key}=${oneLine.length > 80 ? `${oneLine.slice(0, 77)}...` : oneLine}`);
		}
		return parts.join(" ");
	} catch {
		return call.arguments;
	}
}

export function renderToolStart(call: ToolCall, style: Style): string {
	return paint(style, ansi.magenta, `⚙ ${call.name}`) + " " + paint(style, ansi.dim, summariseArguments(call));
}

export function renderToolResult(result: ToolResultMessage, style: Style, maxLines = 8): string {
	const lines = result.text.replace(/\n+$/, "").split("\n");
	const shown = lines.slice(0, maxLines);
	const more = lines.length > maxLines ? [`… (${lines.length - maxLines} more lines)`] : [];
	const code = result.is_error ? ansi.red : ansi.gray;
	return [...shown, ...more].map((line) => paint(style, code, `  ${line}`)).join("\n");
}

export function renderMessage(message: Message, style: Style): string {
	switch (message.role) {
		case "user":
			return renderUser(message.text, style);
		case "assistant": {
			const parts: string[] = [];
			for (const block of message.content) {
				if (block.type === "text" && block.text.trim() !== "") parts.push(renderAssistantText(block.text, style));
				else if (block.type === "thinking" && block.text.trim() !== "") parts.push(renderThinking(block.text, style));
				else if (block.type === "tool_call") parts.push(renderToolStart(block, style));
			}
			if (message.stop_reason.type === "error") parts.push(paint(style, ansi.red, `error: ${message.stop_reason.message}`));
			if (message.stop_reason.type === "aborted") parts.push(paint(style, ansi.yellow, "[aborted]"));
			return parts.join("\n");
		}
		case "tool_result":
			return renderToolResult(message, style);
	}
}

export function formatTokens(n: number): string {
	if (n >= 1_000_000) return `${(n / 1_000_000).toFixed(1)}M`;
	if (n >= 1000) return `${(n / 1000).toFixed(1)}k`;
	return String(n);
}

export function renderStatus(state: State | null, extra: string, style: Style): string {
	if (!state) return paint(style, ansi.gray, "connecting…");
	const context = state.model.context_window > 0 ? Math.round((100 * state.context_tokens) / state.model.context_window) : 0;
	const parts = [
		state.model.id,
		`thinking:${state.thinking}`,
		`ctx:${formatTokens(state.context_tokens)} (${context}%)`,
		`in:${formatTokens(state.usage.input)} out:${formatTokens(state.usage.output)}`,
		`$${state.cost_usd.toFixed(4)}`,
	];
	if (extra) parts.push(extra);
	const line = parts.join("  ");
	return paint(style, ansi.gray, line.length > style.width ? line.slice(0, style.width) : line);
}

/** Number of terminal rows a string occupies when wrapped at [width]. */
export function rowsFor(text: string, width: number): number {
	let rows = 0;
	for (const line of text.split("\n")) {
		const visible = line.replace(/\x1b\[[0-9;]*m/g, "").length;
		rows += Math.max(1, Math.ceil(visible / Math.max(1, width)));
	}
	return rows;
}
