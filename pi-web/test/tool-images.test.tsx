// @vitest-environment happy-dom
// @vitest-environment-options {"url": "http://127.0.0.1:7789/?session=A"}
import { render } from "preact";
import { act } from "preact/test-utils";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { AgentMessage, ImageContent, SubagentInfo } from "../src/protocol.ts";
import { FakeBackend, FakeSocket, session, settle } from "./fake-backend.ts";

let root: HTMLElement;
let backend: FakeBackend;
let state: typeof import("../src/state.ts");

const png: ImageContent = { type: "image", mimeType: "image/png", data: "iVBORw0KGgo=" };
const gif: ImageContent = { type: "image", mimeType: "image/gif", data: "R0lGODlh" };

const readCall = (id: string, path: string, timestamp: number): AgentMessage => ({
	role: "assistant",
	content: [{ type: "toolCall", id, name: "read", arguments: { path } }],
	provider: "prigh",
	model: "faux",
	stopReason: "toolUse",
	timestamp,
});

const readResult = (id: string, images: ImageContent[], timestamp: number): AgentMessage => ({
	role: "toolResult",
	toolCallId: id,
	toolName: "read",
	content: [{ type: "text", text: "Read image file [image/png, 800x600]" }, ...images],
	isError: false,
	timestamp,
});

const info: SubagentInfo = {
	id: "a1",
	label: "look at it",
	state: "running",
	task: "look at it",
	model: "faux",
	callId: "p1",
	parentId: null,
	result: null,
};

beforeEach(async () => {
	vi.stubGlobal("WebSocket", FakeSocket);
	localStorage.clear();
	history.replaceState(null, "", "/?session=A");
	backend = new FakeBackend([
		session("A", {
			messages: [
				{ role: "user", content: [{ type: "text", text: "what is this?" }, gif], timestamp: 0 },
				readCall("t1", "shot.png", 1),
				readResult("t1", [png, gif], 2),
				readCall("t2", "notes.txt", 3),
				{
					role: "toolResult",
					toolCallId: "t2",
					toolName: "read",
					content: [{ type: "text", text: "just text" }],
					isError: false,
					timestamp: 4,
				},
			],
			transcripts: {
				a1: { subagent: info, messages: [readCall("s1", "sub.png", 0), readResult("s1", [png], 1)] },
			},
		}),
	]);
	FakeSocket.backend = backend;
	vi.resetModules();
	state = await import("../src/state.ts");
	state.client.start();
	await settle();
	root = document.createElement("div");
	document.body.appendChild(root);
	const { App } = await import("../src/app.tsx");
	act(() => {
		render(<App />, root);
	});
});

afterEach(() => {
	render(null, root);
	root.remove();
	state.client.stop();
	vi.unstubAllGlobals();
});

/** Each message as the user sees it: text, and images as [alt mime class]. */
function shown(selector = ".msg"): string {
	return [...root.querySelectorAll(selector)]
		.map((message) => {
			const parts: string[] = [];
			const walk = (e: Element) => {
				if (e instanceof HTMLImageElement) {
					const mime = /^data:([^;]+);base64,/.exec(e.getAttribute("src") ?? "")?.[1];
					const where = e.parentElement?.className === "tool-images" ? " in .tool-images" : "";
					parts.push(`[${e.alt} ${mime} .${e.className.replace(/ /g, ".")}${where}]`);
				} else if (e.children.length === 0) {
					const text = (e.textContent ?? "").replace(/\s+/g, " ").trim();
					if (text) parts.push(text);
				} else {
					for (const child of e.children) walk(child);
				}
			};
			walk(message);
			return parts.join(" · ");
		})
		.join("\n");
}

async function push(message: Record<string, unknown>): Promise<void> {
	await act(async () => {
		backend.push(message);
		await settle();
	});
}

describe("images in the chat", () => {
	it("shows tool result images from the history as thumbnails", () => {
		expect(shown()).toMatchInlineSnapshot(`
			"what is this? · [attached image/gif .msg-image]
			faux · read · shot.png · Read image file [image/png, 800x600] · [read result image/png .msg-image in .tool-images] · [read result image/gif .msg-image in .tool-images]
			faux · read · notes.txt · just text"
		`);
		const img = root.querySelector(".tool-images img") as HTMLImageElement;
		expect(img.src).toBe(`data:image/png;base64,${png.data}`);
	});

	it("shows tool result images as the tool finishes", async () => {
		await push({ type: "message_end", message: readCall("t3", "live.png", 5) });
		await push({ type: "tool_execution_start", toolCallId: "t3", toolName: "read", args: { path: "live.png" } });
		expect(shown(".msg:last-child")).toMatchInlineSnapshot(`"faux · read · live.png · …"`);
		await push({
			type: "tool_execution_end",
			toolCallId: "t3",
			toolName: "read",
			result: { content: [{ type: "text", text: "Read image file [image/png, 800x600]" }, png] },
			isError: false,
		});
		expect(shown(".msg:last-child")).toMatchInlineSnapshot(
			`"faux · read · live.png · Read image file [image/png, 800x600] · [read result image/png .msg-image in .tool-images]"`,
		);
		await push({ type: "tool_execution_start", toolCallId: "t3", toolName: "read", args: { path: "live.png" } });
		expect(shown(".msg:last-child")).toMatchInlineSnapshot(`"faux · read · live.png · …"`);
	});

	it("a click shows the image full size and another shrinks it back", async () => {
		const img = () => root.querySelector(".tool-images img") as HTMLImageElement;
		expect([img().className, img().title]).toEqual(["msg-image", "Click for full size"]);
		await act(async () => img().click());
		expect([img().className, img().title]).toEqual(["msg-image full", "Click to shrink"]);
		await act(async () => img().click());
		expect(img().className).toBe("msg-image");
	});

	it("shows them in the subagent view, from its transcript and live", async () => {
		await act(async () => {
			await state.openSubagent({ agentId: "a1" });
			await settle();
		});
		expect(shown(".subagent-view .msg")).toMatchInlineSnapshot(
			`"faux · read · sub.png · Read image file [image/png, 800x600] · [read result image/png .msg-image in .tool-images]"`,
		);
		await push({
			type: "prigh_subagent_event",
			agentId: "a1",
			event: { type: "message_end", message: readCall("s2", "b.gif", 2) },
		});
		await push({
			type: "prigh_subagent_event",
			agentId: "a1",
			event: {
				type: "tool_execution_end",
				toolCallId: "s2",
				toolName: "read",
				result: { content: [gif] },
				isError: false,
			},
		});
		expect(shown(".subagent-view .msg")).toMatchInlineSnapshot(`
			"faux · read · sub.png · Read image file [image/png, 800x600] · [read result image/png .msg-image in .tool-images]
			faux · read · b.gif · [read result image/gif .msg-image in .tool-images]"
		`);
	});
});
