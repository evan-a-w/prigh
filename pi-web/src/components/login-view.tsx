import { useState } from "preact/hooks";
import { capsLockFromEvent } from "../caps-lock.ts";
import { loadCredentials, type Storage, saveCredentials, searchAfterLogin } from "../connection.ts";

export interface LoginViewProps {
	/** Why the form is shown ("" after signing out). */
	error: string;
	storage: Storage;
	/** Called with the query string to load once the credentials are stored. */
	onLogin(search: string): void;
	search: string;
}

/**
 * The backend refused the connection (or the user signed out): ask for the
 * user name (the server's namespace; empty without namespaces) and password
 * (the token), store them and reconnect.
 */
export function LoginView({ error, storage, onLogin, search }: LoginViewProps) {
	const previousUser = loadCredentials(storage).user;
	const [user, setUser] = useState(previousUser);
	const [password, setPassword] = useState("");
	const [capsLock, setCapsLock] = useState(false);
	const trackCapsLock = (event: Event) => {
		const on = capsLockFromEvent(event);
		if (on !== undefined) setCapsLock(on);
	};
	return (
		<div class="unreachable-view">
			<div class="unreachable-card">
				<h1>Sign in to prigh</h1>
				{error ? <p class="connect-error">{error}</p> : null}
				<form
					class="connect-form"
					onSubmit={(event) => {
						event.preventDefault();
						const name = user.trim();
						saveCredentials(storage, { user: name, password: password.trim() });
						onLogin(searchAfterLogin(search, previousUser, name));
					}}
				>
					<label class="connect-field">
						<span>User name</span>
						<input
							name="user"
							type="text"
							value={user}
							autocomplete="username"
							autocapitalize="off"
							spellcheck={false}
							placeholder="Leave empty if the server has no users"
							onInput={(event) => setUser((event.target as HTMLInputElement).value)}
						/>
					</label>
					<label class="connect-field">
						<span>Password</span>
						<input
							name="password"
							type="password"
							value={password}
							autocomplete="current-password"
							placeholder="prigh serve -token …"
							onInput={(event) => setPassword((event.target as HTMLInputElement).value)}
							onKeyDown={trackCapsLock}
							onKeyUp={trackCapsLock}
							onMouseDown={trackCapsLock}
							onBlur={() => setCapsLock(false)}
						/>
					</label>
					{capsLock ? (
						<p class="caps-lock-warning" role="status">
							Caps Lock is on
						</p>
					) : null}
					<button type="submit" class="unreachable-home">
						Sign in
					</button>
				</form>
			</div>
		</div>
	);
}
