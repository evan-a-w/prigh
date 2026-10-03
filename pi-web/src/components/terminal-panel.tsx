import { useEffect, useRef, useState } from "preact/hooks";
import { currentTerminalUrl, terminalOpen } from "../state.ts";

/**
 * A shell on the backend (its `/terminal` WebSocket, see
 * backend/lib/terminals.mli) drawn by the Bonsai web UI's
 * tui/web-bin/terminal.js, which vite.config.ts ships under xterm/. Closing
 * the panel only detaches: the backend keeps the shell for a while, so
 * reopening it in the same session finds it again.
 */

interface TerminalHandle {
	focus(): void;
	dispose(): void;
}

declare global {
	interface Window {
		prighTerminal?: { mount(host: HTMLElement, url: string): TerminalHandle };
	}
}

const MIN_HEIGHT = 180;

let assets: Promise<void> | undefined;

function loadAssets(): Promise<void> {
	const base = "xterm/";
	const script = (name: string) =>
		new Promise<void>((resolve, reject) => {
			const element = document.createElement("script");
			element.src = base + name;
			element.onload = () => resolve();
			element.onerror = () => reject(new Error(`could not load ${name}`));
			document.head.appendChild(element);
		});
	assets ??= (async () => {
		const css = document.createElement("link");
		css.rel = "stylesheet";
		css.href = `${base}xterm.css`;
		document.head.appendChild(css);
		await script("xterm.js");
		await script("addon-fit.js");
		await script("terminal.js");
	})().catch((error) => {
		assets = undefined;
		throw error;
	});
	return assets;
}

function TerminalHost({ url }: { url: string }) {
	const host = useRef<HTMLDivElement>(null);
	const [error, setError] = useState<string | undefined>(undefined);
	useEffect(() => {
		let handle: TerminalHandle | undefined;
		let disposed = false;
		loadAssets().then(
			() => {
				const element = host.current;
				if (disposed || !element || !window.prighTerminal) return;
				handle = window.prighTerminal.mount(element, url);
			},
			(e: unknown) => setError(`terminal unavailable: ${e instanceof Error ? e.message : String(e)}`),
		);
		return () => {
			disposed = true;
			handle?.dispose();
		};
	}, [url]);
	return (
		<div class="terminal-host-wrap">
			<div class="terminal-host" ref={host} />
			{error && <div class="terminal-status-overlay">{error}</div>}
		</div>
	);
}

function ResizeHandle({ onResize }: { onResize: (height: number) => void }) {
	return (
		<button
			type="button"
			class="terminal-resize-handle"
			title="Drag to resize"
			aria-label="Resize terminal"
			onPointerDown={(event) => {
				const panel = (event.currentTarget as HTMLElement).parentElement;
				if (!panel) return;
				event.preventDefault();
				const startY = event.clientY;
				const startHeight = panel.getBoundingClientRect().height;
				const target = event.currentTarget as HTMLElement;
				target.setPointerCapture(event.pointerId);
				document.body.classList.add("terminal-resizing");
				const move = (e: PointerEvent) => {
					const max = window.innerHeight - 120;
					onResize(Math.max(MIN_HEIGHT, Math.min(max, startHeight + startY - e.clientY)));
				};
				const up = () => {
					document.body.classList.remove("terminal-resizing");
					target.removeEventListener("pointermove", move);
					target.removeEventListener("pointerup", up);
					target.removeEventListener("pointercancel", up);
				};
				target.addEventListener("pointermove", move);
				target.addEventListener("pointerup", up);
				target.addEventListener("pointercancel", up);
			}}
		/>
	);
}

export function TerminalPanel() {
	const [height, setHeight] = useState<number | undefined>(undefined);
	if (!terminalOpen.value) return null;
	const url = currentTerminalUrl();
	return (
		<div class="terminal-panel" style={height === undefined ? undefined : { height: `${height}px` }}>
			<ResizeHandle onResize={setHeight} />
			<div class="terminal-header">
				<span class="terminal-title">Terminal</span>
				<button
					type="button"
					class="terminal-close"
					title="Hide (the shell keeps running for a while)"
					onClick={() => {
						terminalOpen.value = false;
					}}
				>
					×
				</button>
			</div>
			<TerminalHost key={url} url={url} />
		</div>
	);
}
