const fs = require("node:fs");

const STATE_FILE = process.argv[2];
const ATTEMPTS = fs.existsSync(STATE_FILE) ? JSON.parse(fs.readFileSync(STATE_FILE, "utf8")) : [];
const ATTEMPT = { pid: process.pid, ready: false, rejected: false };
ATTEMPTS.push(ATTEMPT);

function save() {
  fs.writeFileSync(`${STATE_FILE}.tmp`, JSON.stringify(ATTEMPTS));
  fs.renameSync(`${STATE_FILE}.tmp`, STATE_FILE);
}

save();

function respond(message) {
  const body = JSON.stringify({ jsonrpc: "2.0", ...message });
  process.stdout.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
}

function receive(message) {
  if (message.method === "initialize") {
    if (ATTEMPTS.length === 2) {
      ATTEMPT.rejected = true;
      save();
      respond({
        id: message.id,
        error: { code: -32603, message: "Restart initialization failed" },
      });
    } else {
      respond({ id: message.id, result: { capabilities: {} } });
    }
  } else if (message.method === "initialized") {
    ATTEMPT.ready = true;
    save();
  } else if (message.method === "shutdown") {
    respond({ id: message.id, result: null });
  } else if (message.method === "exit") {
    process.exit(0);
  }
}

let input = Buffer.alloc(0);
process.stdin.on("data", (chunk) => {
  input = Buffer.concat([input, chunk]);
  for (;;) {
    const headerEnd = input.indexOf("\r\n\r\n");
    if (headerEnd < 0) return;
    const length = Number(
      /Content-Length: (\d+)/i.exec(input.subarray(0, headerEnd).toString())[1],
    );
    const end = headerEnd + 4 + length;
    if (input.length < end) return;
    const message = JSON.parse(input.subarray(headerEnd + 4, end).toString());
    input = input.subarray(end);
    receive(message);
  }
});
