// The name the backend and its tool hosts give this machine: prigh_web.sh
// sets PRIGH_HOST_NAME, so the output is the same on every machine (and a
// machine named like a word, "desktop", does not rewrite that word).
import { hostname } from "node:os";

export const hostName = () => process.env.PRIGH_HOST_NAME || hostname();
