// @vitest-environment happy-dom
import { render } from "preact";
import { act } from "preact/test-utils";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { ProviderLoginDialog } from "../src/components/provider-login-dialog.tsx";
import type { AgentMessage, RpcExtensionUIRequest } from "../src/protocol.ts";
import { type ProviderLogin, type ProviderLoginInput, reduce } from "../src/provider-login.ts";

const URL_ = "https://claude.ai/oauth/authorize?code=true&client_id=x&state=abc";

// What pi_rpc.ml sends for an Anthropic OAuth login.
const loginMessage: AgentMessage = {
	role: "custom",
	customType: "login",
	content: `Complete login in your browser. If the browser is on another machine, paste the final redirect URL here.\n\n[${URL_}](${URL_})`,
	display: true,
	timestamp: 3,
};
const codePrompt: RpcExtensionUIRequest = {
	type: "extension_ui_request",
	id: "auth-p1",
	method: "input",
	title: "Complete login in your browser, or paste the authorization code / redirect URL here:",
	placeholder: "https://console.anthropic.com/oauth/code/callback",
};
const notify = (message: string, notifyType?: "info" | "error"): RpcExtensionUIRequest => ({
	type: "extension_ui_request",
	id: "notify-1",
	method: "notify",
	message,
	...(notifyType ? { notifyType } : {}),
});

function run(inputs: ProviderLoginInput[], start?: ProviderLogin) {
	let login = start;
	return inputs.map((input) => {
		const reduced = reduce(login, input);
		login = reduced.login;
		const label =
			input.kind === "message"
				? `message ${input.message.role}`
				: input.kind === "ui_request"
					? `${input.request.method} ${"message" in input.request ? input.request.message : input.request.id}`
					: input.kind === "ui_cancel"
						? `cancel ${input.id}`
						: input.kind;
		const state = login
			? [
					login.request ? `prompt ${login.request.id}` : "no prompt",
					login.progress && `progress: ${login.progress}`,
					login.error && `error: ${login.error}`,
				]
					.filter(Boolean)
					.join(", ")
			: "closed";
		return `${label} -> ${reduced.consumed ? "dialog" : "elsewhere"} | ${state}`;
	});
}

describe("reduce", () => {
	it("pulls the link out of the login message", () => {
		expect(reduce(undefined, { kind: "message", message: loginMessage }).login).toEqual({
			url: URL_,
			instructions:
				"Complete login in your browser. If the browser is on another machine, paste the final redirect URL here.",
		});
	});

	it("a pasted code: prompt, progress, success closes", () => {
		expect(
			run([
				{ kind: "message", message: loginMessage },
				{ kind: "ui_request", request: codePrompt },
				{ kind: "responded" },
				{ kind: "ui_request", request: notify("Exchanging authorization code for tokens...") },
				{ kind: "ui_request", request: notify("logged in to anthropic (oauth)") },
			]),
		).toMatchInlineSnapshot(`
			[
			  "message custom -> dialog | no prompt",
			  "input auth-p1 -> dialog | prompt auth-p1",
			  "responded -> dialog | no prompt, progress: Checking the code…",
			  "notify Exchanging authorization code for tokens... -> dialog | no prompt, progress: Exchanging authorization code for tokens...",
			  "notify logged in to anthropic (oauth) -> elsewhere | closed",
			]
		`);
	});

	it("the loopback redirect cancels the prompt; a failure stays in the dialog", () => {
		expect(
			run([
				{ kind: "message", message: loginMessage },
				{ kind: "ui_request", request: codePrompt },
				{ kind: "ui_cancel", id: "other" },
				{ kind: "ui_cancel", id: "auth-p1" },
				{ kind: "ui_request", request: notify("login to anthropic failed: token exchange: 400", "error") },
				{ kind: "dismiss" },
			]),
		).toMatchInlineSnapshot(`
			[
			  "message custom -> dialog | no prompt",
			  "input auth-p1 -> dialog | prompt auth-p1",
			  "cancel other -> elsewhere | prompt auth-p1",
			  "cancel auth-p1 -> dialog | no prompt",
			  "notify login to anthropic failed: token exchange: 400 -> dialog | no prompt, error: login to anthropic failed: token exchange: 400",
			  "dismiss -> dialog | closed",
			]
		`);
	});

	it("without a login in progress everything goes elsewhere (API key prompts, toasts, chat)", () => {
		const other: AgentMessage = { ...loginMessage, customType: "auth", content: "Providers: [x](y)" };
		expect(
			run([
				{ kind: "message", message: other },
				{ kind: "ui_request", request: { ...codePrompt, title: "Enter DeepSeek API key" } },
				{ kind: "ui_request", request: notify("login to deepseek failed: login cancelled", "error") },
				{ kind: "ui_cancel", id: "auth-p1" },
				{ kind: "responded" },
			]),
		).toMatchInlineSnapshot(`
			[
			  "message custom -> elsewhere | closed",
			  "input auth-p1 -> elsewhere | closed",
			  "notify login to deepseek failed: login cancelled -> elsewhere | closed",
			  "cancel auth-p1 -> elsewhere | closed",
			  "responded -> elsewhere | closed",
			]
		`);
	});

	it("other dialogs during a login are not swallowed", () => {
		const select: RpcExtensionUIRequest = {
			type: "extension_ui_request",
			id: "dialog-2",
			method: "select",
			title: "t",
			options: [],
		};
		const started = reduce(undefined, { kind: "message", message: loginMessage }).login;
		expect(run([{ kind: "ui_request", request: select }], started)).toEqual(["select dialog-2 -> elsewhere | no prompt"]);
	});
});

describe("ProviderLoginDialog", () => {
	let root: HTMLElement;
	beforeEach(() => {
		root = document.createElement("div");
		document.body.appendChild(root);
	});
	afterEach(() => {
		render(null, root);
		root.remove();
	});

	function mount(login: ProviderLogin, copyResult = true) {
		const calls: string[] = [];
		act(() => {
			render(
				<ProviderLoginDialog
					login={login}
					onSubmit={(code) => calls.push(`submit ${code}`)}
					onClose={() => calls.push("close")}
					copy={async (text) => {
						calls.push(`copy ${text}`);
						return copyResult;
					}}
				/>,
				root,
			);
		});
		return calls;
	}

	const started = reduce(undefined, { kind: "message", message: loginMessage }).login as ProviderLogin;
	const prompting = reduce(started, { kind: "ui_request", request: codePrompt }).login as ProviderLogin;
	const text = () =>
		[...root.querySelectorAll(".dialog > *, li, a, button, input")]
			.filter((e) => !e.matches("ol, form, .provider-login-link, .dialog-actions"))
			.map((e) => {
				if (e instanceof HTMLAnchorElement) return `<a href=${e.href} target=${e.target} rel=${e.rel}>`;
				if (e instanceof HTMLInputElement) return `[input placeholder=${e.placeholder}]`;
				if (e instanceof HTMLButtonElement) return `[${e.textContent}]`;
				return e.textContent;
			})
			.join("\n");

	it("shows the link inside the dialog with the instructions and the code field", () => {
		mount(prompting);
		expect(text()).toMatchInlineSnapshot(`
			"Log in to a provider
			Complete login in your browser. If the browser is on another machine, paste the final redirect URL here.
			Open the link and sign in.
			Paste the code or the full redirect URL below.
			<a href=https://claude.ai/oauth/authorize?code=true&client_id=x&state=abc target=_blank rel=noopener noreferrer>
			[Copy link]
			[input placeholder=https://console.anthropic.com/oauth/code/callback]
			[Submit]
			[Cancel]"
		`);
	});

	it("copies the link", async () => {
		const calls = mount(prompting);
		await act(async () => {
			root.querySelector<HTMLButtonElement>(".provider-login-copy")?.click();
		});
		expect(calls).toEqual([`copy ${URL_}`]);
		expect(root.querySelector(".provider-login-copy")?.textContent).toBe("Copied");
	});

	it("says when copying failed", async () => {
		mount(prompting, false);
		await act(async () => {
			root.querySelector<HTMLButtonElement>(".provider-login-copy")?.click();
		});
		expect(root.querySelector(".provider-login-copy")?.textContent).toBe("Copy failed");
	});

	it("submits the trimmed code, ignores an empty one; Cancel closes", () => {
		const calls = mount(prompting);
		const form = root.querySelector("form") as HTMLFormElement;
		const submit = () =>
			act(() => {
				form.dispatchEvent(new Event("submit", { bubbles: true, cancelable: true }));
			});
		submit();
		act(() => {
			const field = root.querySelector("input") as HTMLInputElement;
			field.value = "  abc#state ";
			field.dispatchEvent(new Event("input", { bubbles: true }));
		});
		submit();
		act(() => {
			[...root.querySelectorAll("button")].find((b) => b.textContent === "Cancel")?.click();
		});
		expect(calls).toEqual(["submit abc#state", "close"]);
	});

	it("shows progress, then errors inside the dialog, with only a Close button once the prompt is gone", () => {
		const waiting = reduce(prompting, { kind: "responded" }).login as ProviderLogin;
		mount(waiting);
		expect(root.querySelector(".provider-login-progress")?.textContent).toBe("Checking the code…");
		const failed = reduce(waiting, {
			kind: "ui_request",
			request: notify("login to anthropic failed: OAuth state mismatch", "error"),
		}).login as ProviderLogin;
		const calls = mount(failed);
		expect(text()).toContain("login to anthropic failed: OAuth state mismatch\n[Close]");
		expect(root.querySelector("input")).toBeNull();
		expect(root.querySelector("[role=alert]")?.textContent).toBe("login to anthropic failed: OAuth state mismatch");
		act(() => {
			root.querySelector<HTMLButtonElement>(".dialog-actions button")?.click();
		});
		expect(calls).toEqual(["close"]);
	});
});
