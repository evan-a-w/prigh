import type { Storage } from "../src/connection.ts";

export function memoryStorage(initial: Record<string, string> = {}): Storage & { data: Record<string, string> } {
	const data = { ...initial };
	return {
		data,
		getItem: (key) => data[key] ?? null,
		setItem: (key, value) => {
			data[key] = value;
		},
		removeItem: (key) => {
			delete data[key];
		},
	};
}
