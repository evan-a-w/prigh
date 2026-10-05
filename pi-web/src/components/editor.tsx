import { computed, signal } from "@preact/signals";
import { useEffect, useRef } from "preact/hooks";
import { prepareImageFile } from "../image-file.ts";
import type { ImageContent, RpcSlashCommand } from "../protocol.ts";
import {
	editorText,
	executeBuiltinCommand,
	pushToast,
	queue,
	sendAbort,
	sendBash,
	sendPrompt,
	sessionState,
	slashCommands,
	workingMessage,
} from "../state.ts";
import { imageSrc } from "./image-thumb.tsx";

const TUI_BUILTIN_COMMAND_ORDER = new Map(
	[
		"model",
		"thinking",
		"new",
		"name",
		"session",
		"fork",
		"clone",
		"cd",
		"compact",
		"export",
		"copy",
	].map((name, index): [string, number] => [name, index]),
);
const TUI_SOURCE_ORDER = {
	builtin: 0,
	prompt: 1,
	extension: 2,
	skill: 3,
} satisfies Record<RpcSlashCommand["source"], number>;

const pendingImages = signal<ImageContent[]>([]);
const autocompleteIndex = signal(0);
const autocompleteDismissed = signal(false);
const sending = signal(false);

/**
 * Composer draft, local to this module (Editor is a singleton component).
 * Deliberately NOT the shared `editorText` signal: writing that signal on
 * every keystroke would make it a shared dependency other components could
 * accidentally subscribe to, turning every keystroke into an app-wide
 * re-render trigger. Only this file (and the autocomplete computeds below)
 * reads `draftText`; `editorText` remains as a one-way channel FROM the rest
 * of the app INTO the composer (extension `set_editor_text` requests, /fork),
 * synced into `draftText` by the effect in Editor below.
 */
const draftText = signal("");

function fuzzyMatch(query: string, text: string): { matches: boolean; score: number } {
	const queryLower = query.toLowerCase();
	const textLower = text.toLowerCase();

	const matchQuery = (normalizedQuery: string): { matches: boolean; score: number } => {
		if (normalizedQuery.length === 0) {
			return { matches: true, score: 0 };
		}
		if (normalizedQuery.length > textLower.length) {
			return { matches: false, score: 0 };
		}

		let queryIndex = 0;
		let score = 0;
		let lastMatchIndex = -1;
		let consecutiveMatches = 0;

		for (let i = 0; i < textLower.length && queryIndex < normalizedQuery.length; i += 1) {
			if (textLower[i] === normalizedQuery[queryIndex]) {
				const isWordBoundary = i === 0 || /[\s_./:-]/.test(textLower[i - 1] ?? "");
				if (lastMatchIndex === i - 1) {
					consecutiveMatches += 1;
					score -= consecutiveMatches * 5;
				} else {
					consecutiveMatches = 0;
					if (lastMatchIndex >= 0) {
						score += (i - lastMatchIndex - 1) * 2;
					}
				}
				if (isWordBoundary) {
					score -= 10;
				}
				score += i * 0.1;
				lastMatchIndex = i;
				queryIndex += 1;
			}
		}

		if (queryIndex < normalizedQuery.length) {
			return { matches: false, score: 0 };
		}
		if (normalizedQuery === textLower) {
			score -= 100;
		}
		return { matches: true, score };
	};

	const primaryMatch = matchQuery(queryLower);
	if (primaryMatch.matches) {
		return primaryMatch;
	}

	const alphaNumericMatch = /^(?<letters>[a-z]+)(?<digits>[0-9]+)$/.exec(queryLower);
	const numericAlphaMatch = /^(?<digits>[0-9]+)(?<letters>[a-z]+)$/.exec(queryLower);
	const swappedQuery = alphaNumericMatch
		? `${alphaNumericMatch.groups?.digits ?? ""}${alphaNumericMatch.groups?.letters ?? ""}`
		: numericAlphaMatch
			? `${numericAlphaMatch.groups?.letters ?? ""}${numericAlphaMatch.groups?.digits ?? ""}`
			: "";
	if (!swappedQuery) {
		return primaryMatch;
	}

	const swappedMatch = matchQuery(swappedQuery);
	return swappedMatch.matches ? { matches: true, score: swappedMatch.score + 5 } : primaryMatch;
}

function fuzzyFilter<T>(items: T[], query: string, getText: (item: T) => string): T[] {
	const tokens = query
		.trim()
		.split(/[\s/]+/)
		.filter((token) => token.length > 0);
	if (tokens.length === 0) {
		return items;
	}

	const results: Array<{ item: T; totalScore: number }> = [];
	for (const item of items) {
		let totalScore = 0;
		let allMatch = true;
		for (const token of tokens) {
			const match = fuzzyMatch(token, getText(item));
			if (!match.matches) {
				allMatch = false;
				break;
			}
			totalScore += match.score;
		}
		if (allMatch) {
			results.push({ item, totalScore });
		}
	}

	results.sort((a, b) => a.totalScore - b.totalScore);
	return results.map((result) => result.item);
}

function getCommandSortIndex(command: RpcSlashCommand): number {
	if (command.source !== "builtin") {
		return 0;
	}
	return TUI_BUILTIN_COMMAND_ORDER.get(command.name) ?? TUI_BUILTIN_COMMAND_ORDER.size;
}

function sortCommandsLikeTui(commands: RpcSlashCommand[]): RpcSlashCommand[] {
	return commands
		.map((command, index) => ({ command, index }))
		.sort((a, b) => {
			const sourceDelta = TUI_SOURCE_ORDER[a.command.source] - TUI_SOURCE_ORDER[b.command.source];
			if (sourceDelta !== 0) {
				return sourceDelta;
			}
			const commandDelta = getCommandSortIndex(a.command) - getCommandSortIndex(b.command);
			return commandDelta !== 0 ? commandDelta : a.index - b.index;
		})
		.map((entry) => entry.command);
}

function formatAutocompleteDescription(command: RpcSlashCommand): string | undefined {
	if (!command.argumentHint) {
		return command.description;
	}
	return command.description ? `${command.argumentHint} — ${command.description}` : command.argumentHint;
}

const autocompleteQuery = computed(() => {
	const text = draftText.value;
	if (!text.startsWith("/") || text.includes(" ") || autocompleteDismissed.value) {
		return undefined;
	}
	return text.slice(1);
});

const autocompleteMatches = computed(() => {
	const query = autocompleteQuery.value;
	if (query === undefined) return [];
	return fuzzyFilter(sortCommandsLikeTui(slashCommands.value), query, (command) => command.name);
});

const autocompleteOpen = computed(() => autocompleteMatches.value.length > 0);

function getSelectedAutocompleteIndex(matches: RpcSlashCommand[]): number {
	return matches.length === 0 ? -1 : Math.min(autocompleteIndex.value, matches.length - 1);
}

const isBusy = computed(() => sessionState.value?.isStreaming === true || workingMessage.value !== undefined);

// On touch devices Enter inserts a newline (no Shift key on virtual keyboards);
// the send button is the send affordance. On desktop Enter sends.
const isCoarsePointer = window.matchMedia("(pointer: coarse)").matches;

/** Attaches the image files among `files`, downscaled or converted as the backend needs. */
function attachImages(files: File[]): void {
	for (const file of files) {
		if (!file.type.startsWith("image/")) continue;
		void prepareImageFile(file)
			.then((fitted) => {
				if (fitted.ok) pendingImages.value = [...pendingImages.value, fitted.image];
				else pushToast(fitted.error, "error");
			})
			.catch(() => pushToast(`Failed to read ${file.name || "the image"}`, "error"));
	}
}

async function send(): Promise<void> {
	const text = draftText.value.trim();
	const images = pendingImages.value;
	if (sending.value || (text === "" && images.length === 0)) return;
	sending.value = true;
	draftText.value = "";
	pendingImages.value = [];
	autocompleteDismissed.value = false;
	try {
		if (text.startsWith("!")) {
			await sendBash(text.slice(1));
			return;
		}
		if (text.startsWith("/") && (await executeBuiltinCommand(text))) {
			return;
		}
		await sendPrompt(text, images);
	} catch (error) {
		pushToast(error instanceof Error ? error.message : String(error), "error");
	} finally {
		sending.value = false;
	}
}

export function Editor() {
	const textareaRef = useRef<HTMLTextAreaElement>(null);
	const autocompleteListRef = useRef<HTMLDivElement>(null);

	const autoGrow = () => {
		const el = textareaRef.current;
		if (!el) return;
		el.style.height = "auto";
		el.style.height = `${Math.min(el.scrollHeight, 200)}px`;
	};

	// External writers (extension `set_editor_text`, /fork) publish through the
	// shared `editorText` signal; adopt those into the local draft. Typing never
	// writes `editorText`, so this only fires on those rare external pushes.
	const pushedText = editorText.value;
	useEffect(() => {
		if (pushedText !== draftText.peek()) {
			draftText.value = pushedText;
			queueMicrotask(autoGrow);
		}
	}, [pushedText]);

	const selectedAutocompleteIndex = getSelectedAutocompleteIndex(autocompleteMatches.value);
	useEffect(() => {
		autocompleteListRef.current?.querySelector(".autocomplete-entry.selected")?.scrollIntoView({ block: "nearest" });
	}, [selectedAutocompleteIndex]);

	const handleKeyDown = (event: KeyboardEvent) => {
		if (autocompleteOpen.value) {
			const matches = autocompleteMatches.value;
			const selectedIndex = getSelectedAutocompleteIndex(matches);
			if (event.key === "ArrowDown") {
				event.preventDefault();
				autocompleteIndex.value = (selectedIndex + 1) % matches.length;
				return;
			}
			if (event.key === "ArrowUp") {
				event.preventDefault();
				autocompleteIndex.value = (selectedIndex - 1 + matches.length) % matches.length;
				return;
			}
			if (event.key === "Tab" || event.key === "Enter") {
				event.preventDefault();
				const selected = matches[selectedIndex];
				if (selected) {
					draftText.value = `/${selected.name} `;
					autocompleteDismissed.value = true;
					autoGrow();
				}
				return;
			}
			if (event.key === "Escape") {
				event.preventDefault();
				autocompleteDismissed.value = true;
				return;
			}
		}
		if (event.key === "Enter" && !event.shiftKey && !isCoarsePointer) {
			event.preventDefault();
			void send();
			return;
		}
		if (event.key === "Escape" && isBusy.value) {
			event.preventDefault();
			void sendAbort();
		}
	};

	const handlePaste = (event: ClipboardEvent) => {
		const files = [...(event.clipboardData?.files ?? [])];
		if (files.length === 0) return;
		event.preventDefault();
		attachImages(files);
	};

	const handleDragOver = (event: DragEvent) => {
		if (event.dataTransfer?.types.includes("Files")) event.preventDefault();
	};

	const handleDrop = (event: DragEvent) => {
		const files = [...(event.dataTransfer?.files ?? [])];
		if (files.length === 0) return;
		event.preventDefault();
		attachImages(files);
	};

	const queuedMessages = [...queue.value.steering, ...queue.value.followUp];

	return (
		<div class="editor-area" onDragOver={handleDragOver} onDrop={handleDrop}>
			{queuedMessages.length > 0 && (
				<div class="queue-indicator">
					{queuedMessages.map((text, index) => (
						<div key={`${index}-${text}`} class="queue-entry">
							queued: {text}
						</div>
					))}
				</div>
			)}
			{pendingImages.value.length > 0 && (
				<div class="pending-images">
					{pendingImages.value.map((image, index) => (
						<button
							key={`${image.mimeType}-${index}`}
							type="button"
							class="pending-image"
							title="Remove image"
							onClick={() => {
								pendingImages.value = pendingImages.value.filter((_, i) => i !== index);
							}}
						>
							<img src={imageSrc(image)} alt="attachment" />
						</button>
					))}
				</div>
			)}
			{autocompleteOpen.value && (
				<div class="autocomplete" ref={autocompleteListRef}>
					{autocompleteMatches.value.map((command, index) => (
						<button
							key={`${command.source}:${command.name}`}
							type="button"
							class={`autocomplete-entry ${index === getSelectedAutocompleteIndex(autocompleteMatches.value) ? "selected" : ""}`}
							onMouseDown={(event) => {
								event.preventDefault();
								draftText.value = `/${command.name} `;
								autocompleteDismissed.value = true;
								textareaRef.current?.focus();
							}}
						>
							<span class="autocomplete-name">/{command.name}</span>
							{formatAutocompleteDescription(command) && (
								<span class="autocomplete-description">{formatAutocompleteDescription(command)}</span>
							)}
						</button>
					))}
				</div>
			)}
			<div class="editor-row">
				<textarea
					ref={textareaRef}
					class="editor-input"
					placeholder={isBusy.value ? "Steer the agent…" : "Send a message…"}
					rows={1}
					enterkeyhint={isCoarsePointer ? "enter" : "send"}
					value={draftText.value}
					onInput={(event) => {
						draftText.value = (event.target as HTMLTextAreaElement).value;
						autocompleteIndex.value = 0;
						if (!draftText.value.startsWith("/")) {
							autocompleteDismissed.value = false;
						}
						autoGrow();
					}}
					onKeyDown={handleKeyDown}
					onPaste={handlePaste}
				/>
				{isBusy.value ? (
					<button type="button" class="editor-button abort" title="Abort (Esc)" onClick={() => void sendAbort()}>
						■
					</button>
				) : null}
				<button type="button" class="editor-button send" title="Send" onClick={() => void send()}>
					<svg width="13" height="13" viewBox="0 0 16 16" fill="none" aria-hidden="true">
						<title>Send</title>
						<path
							d="M3 8h10M9 4l4 4-4 4"
							stroke="currentColor"
							stroke-width="1.6"
							stroke-linecap="round"
							stroke-linejoin="round"
						/>
					</svg>
					Send
				</button>
			</div>
		</div>
	);
}
