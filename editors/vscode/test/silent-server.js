const fs = require("node:fs");

fs.writeFileSync(process.argv[2], String(process.pid));
process.stdin.resume();
