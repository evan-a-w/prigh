// Gates: a bash call that waits for a file the script creates, so that what
// runs when is fixed (the faux script's replies are taken in order by
// whichever agent asks next). The backend moves a foreground command to the
// background after `auto_background_seconds` (30s by default) and the agent
// carries on, taking the next reply out of turn: on a slow browser or a busy
// machine a gate may be held longer than that, so the backends of scripts
// with gates never move commands to the background.
// Call holdGates with a backend's HOME before starting the backend.
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

// Each waits at most as long as a script may run, and every gate is opened
// at teardown, so no shell is left polling (#67).
export const waitFor = file =>
  `timeout 300 sh -c 'until [ -f ${file} ]; do sleep 0.1; done'; echo ${file}`;

export const holdGates = home => {
  const dir = join(home, ".prigh");
  mkdirSync(dir, { recursive: true });
  const file = join(dir, "config.json");
  let config = {};
  try { config = JSON.parse(readFileSync(file, "utf8")); } catch {}
  writeFileSync(file, JSON.stringify({ ...config, auto_background_seconds: 0 }) + "\n");
};
