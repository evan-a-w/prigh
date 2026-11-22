// The name the backends give their own host (the host chip, the cards, the
// tool calls' "on HOST"): prigh_web.sh fixes it in $PRIGH_HOST_NAME, so that
// what the pages show, and where (the chip's width moves its menu), is the
// same on every machine.
import { hostname } from "node:os";

export const hostName = () => process.env.PRIGH_HOST_NAME || hostname();
