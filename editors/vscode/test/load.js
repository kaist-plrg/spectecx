const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function load(name, mocks, globals = {}) {
  const filename = path.join(__dirname, "..", name);
  const module = { exports: {} };
  vm.runInNewContext(
    fs.readFileSync(filename, "utf8"),
    {
      module,
      exports: module.exports,
      require: (dependency) => mocks[dependency] || require(dependency),
      process,
      setTimeout,
      clearTimeout,
      console,
      __dirname: path.dirname(filename),
      ...globals,
    },
    { filename },
  );
  return module.exports;
}

function deferred() {
  let resolve, reject;
  const promise = new Promise((yes, no) => {
    resolve = yes;
    reject = no;
  });
  return { promise, resolve, reject };
}

const flush = () => new Promise(setImmediate);
module.exports = { load, deferred, flush };
