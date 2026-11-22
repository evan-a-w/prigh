// prigh-web's verbosity is quiet by default (quiet.mjs checks it); the other
// scripts read the transcript unfolded, as a user who chose normal would.
// Call it with a backend's HOME before starting the backend.
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

export const normalVerbosity = home => {
  const dir = join(home, ".prigh");
  mkdirSync(dir, { recursive: true });
  const file = join(dir, "prefs.json");
  let prefs = {};
  try { prefs = JSON.parse(readFileSync(file, "utf8")); } catch {}
  writeFileSync(file, JSON.stringify({ ...prefs, "web.verbosity": "normal" }) + "\n");
};
