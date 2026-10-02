/** Reading prigh's tool arguments for display. */

export function summarizeArgs(args: Record<string, unknown>): string {
	const candidate =
		(typeof args.command === "string" && args.command) ||
		(typeof args.path === "string" && args.path) ||
		(typeof args.file_path === "string" && args.file_path) ||
		(typeof args.query === "string" && args.query) ||
		(typeof args.pattern === "string" && args.pattern) ||
		(typeof args.task === "string" && args.task) ||
		JSON.stringify(args);
	const singleLine = candidate.replace(/\s+/g, " ").trim();
	return singleLine.length > 100 ? `${singleLine.slice(0, 100)}…` : singleLine;
}

interface EditArg {
	old_text: string;
	new_text: string;
}

/** prigh's edit tool takes `{path, edits: [{old_text, new_text}]}`. */
export function editsOf(args: Record<string, unknown>): EditArg[] {
	if (!Array.isArray(args.edits)) return [];
	return args.edits.filter(
		(edit): edit is EditArg =>
			typeof edit === "object" &&
			edit !== null &&
			typeof (edit as EditArg).old_text === "string" &&
			typeof (edit as EditArg).new_text === "string",
	);
}

/** Lines the diff view adds for a tool call (for the collapse threshold). */
export function diffLineCount(name: string, args: Record<string, unknown>): number {
	if (name === "edit") {
		return editsOf(args).reduce((total, edit) => total + edit.new_text.split("\n").length, 0);
	}
	if (name === "write" && typeof args.content === "string") {
		return args.content.split("\n").length;
	}
	return 0;
}
