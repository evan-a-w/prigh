// A minimal MCP server for mcp_settings.mjs: newline-delimited JSON-RPC on
// stdin and stdout, with one tool, "echo" (its description is the server's
// name, from argv). The one named "echo" starts only when $TOKEN equals
// $FAKE_MCP_TOKEN, showing that the settings' env reaches it.
import { createInterface } from "node:readline";

const [name = "fake"] = process.argv.slice(2);
if (name === "echo" && process.env.TOKEN !== process.env.FAKE_MCP_TOKEN) {
  process.stderr.write(`${name}: wrong TOKEN\n`);
  process.exit(1);
}
const send = message => process.stdout.write(`${JSON.stringify({ jsonrpc: "2.0", ...message })}\n`);
for await (const line of createInterface({ input: process.stdin })) {
  if (!line.trim()) continue;
  const { id, method, params } = JSON.parse(line);
  if (id === undefined) continue;
  switch (method) {
    case "initialize":
      send({ id, result: { protocolVersion: params?.protocolVersion ?? "2025-06-18", capabilities: { tools: {} }, serverInfo: { name, version: "1" } } });
      break;
    case "tools/list":
      send({ id, result: { tools: [{ name: "echo", description: `Echoes, from ${name}`, inputSchema: { type: "object", properties: { text: { type: "string" } } } }] } });
      break;
    case "tools/call":
      send({ id, result: { content: [{ type: "text", text: String(params?.arguments?.text ?? "") }] } });
      break;
    case "ping":
      send({ id, result: {} });
      break;
    default:
      send({ id, error: { code: -32601, message: `no method ${method}` } });
  }
}
