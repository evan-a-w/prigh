import { useState } from "preact/hooks";
import type { ProviderLogin } from "../provider-login.ts";

export interface ProviderLoginDialogProps {
	login: ProviderLogin;
	onSubmit(code: string): void;
	onClose(): void;
	copy(text: string): Promise<boolean>;
}

/** The OAuth link, the code prompt and the login's progress in one dialog (see provider-login.ts). */
export function ProviderLoginDialog({ login, onSubmit, onClose, copy }: ProviderLoginDialogProps) {
	const [code, setCode] = useState("");
	const [copied, setCopied] = useState<boolean | undefined>(undefined);
	const { request } = login;
	return (
		<div class="dialog provider-login" role="dialog" aria-label="Log in to a provider">
			<div class="dialog-title">Log in to a provider</div>
			{login.instructions ? <div class="dialog-message">{login.instructions}</div> : null}
			<ol class="provider-login-steps">
				<li>Open the link and sign in.</li>
				<li>Paste the code or the full redirect URL below.</li>
			</ol>
			<div class="provider-login-link">
				<a href={login.url} target="_blank" rel="noopener noreferrer">
					{login.url}
				</a>
				<button
					type="button"
					class="dialog-button provider-login-copy"
					onClick={() => void copy(login.url).then(setCopied)}
				>
					{copied === true ? "Copied" : copied === false ? "Copy failed" : "Copy link"}
				</button>
			</div>
			{login.error ? (
				<div class="provider-login-error" role="alert">
					{login.error}
				</div>
			) : login.progress ? (
				<div class="provider-login-progress">{login.progress}</div>
			) : null}
			{request ? (
				<form
					onSubmit={(event) => {
						event.preventDefault();
						if (code.trim() === "") return;
						onSubmit(code.trim());
						setCode("");
					}}
				>
					<input
						class="dialog-input"
						type="text"
						aria-label={request.title}
						placeholder={request.placeholder || "Code or redirect URL"}
						value={code}
						autocomplete="off"
						spellcheck={false}
						onInput={(event) => setCode((event.target as HTMLInputElement).value)}
					/>
					<div class="dialog-actions">
						<button type="submit" class="dialog-button">
							Submit
						</button>
						<button type="button" class="dialog-button" onClick={onClose}>
							Cancel
						</button>
					</div>
				</form>
			) : (
				<div class="dialog-actions">
					<button type="button" class="dialog-button" onClick={onClose}>
						Close
					</button>
				</div>
			)}
		</div>
	);
}
