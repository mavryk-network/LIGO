import commonjs from "@rollup/plugin-commonjs";
import resolve from "@rollup/plugin-node-resolve";

export default {
  input: "./editor.js",
  output: {
    file: "./editor.bundle.js",
    format: "iife",
  },
  plugins: [resolve(), commonjs()],
};
