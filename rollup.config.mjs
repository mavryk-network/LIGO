import commonjs from "@rollup/plugin-commonjs";
import resolve from "@rollup/plugin-node-resolve";
import nodePolyfills from "rollup-plugin-polyfill-node";
import json from "@rollup/plugin-json";

export default {
  input: "./demo-webide.js",
  output: {
    file: "./_demo-webide_build/demo-webide.bundle.js",
    format: "iife",
  },
  plugins: [json(), commonjs(), resolve(), nodePolyfills(/* options */)],
};
