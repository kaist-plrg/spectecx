const js = require("@eslint/js");

module.exports = [
  js.configs.recommended,
  {
    files: ["**/*.js"],
    languageOptions: {
      ecmaVersion: 2022,
      sourceType: "commonjs",
      globals: Object.fromEntries(
        [
          "__dirname",
          "Buffer",
          "console",
          "process",
          "setTimeout",
          "clearTimeout",
          "setImmediate",
        ].map((name) => [name, "readonly"]),
      ),
    },
    rules: {
      eqeqeq: "error",
      "no-var": "error",
      "prefer-const": "error",
      "no-unused-vars": ["error", { caughtErrorsIgnorePattern: "^_" }],
    },
  },
];
