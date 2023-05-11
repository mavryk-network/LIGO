/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */

// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  "docs": {
    "Getting started": [
      "intro/introduction",
      "tutorials/getting-started/getting-started",
      "intro/installation",
    ],
    "Basics": [
      "language-basics/types"
    ],
    "Writing a Contract": [
      "advanced/entrypoints-contracts"
    ],
    "Testing and Debugging": [
      "advanced/testing",
      "advanced/mutation-testing",
      "advanced/michelson_testing",
    ],
    "Combining Code": [
      "language-basics/modules",
      "advanced/global-constants",
      "advanced/package-management"
    ],
    "Advanced Topics": [
      "advanced/polymorphism",
      "advanced/inline",
    ]
  },
  "API": {
    "CLI": [
      "manpages/changelog",
      "manpages/add-user",
      "manpages/analytics accept",
      "manpages/analytics deny",
      "manpages/compile constant",
      "manpages/compile contract",
      "manpages/compile expression",
      "manpages/compile parameter",
      "manpages/compile storage",
      "manpages/info get-scope",
      "manpages/info list-declarations",
      "manpages/info measure-contract",
      "manpages/init contract",
      "manpages/init library",
      "manpages/install",
      "manpages/ligo",
      "manpages/login",
      "manpages/mutate cst",
      "manpages/print ast-aggregated",
      "manpages/print ast-core",
      "manpages/print ast-typed",
      "manpages/print ast-expanded",
      "manpages/print ast-unified",
      "manpages/print cst",
      "manpages/print dependency-graph",
      "manpages/print mini-c",
      "manpages/print preprocessed",
      "manpages/print pretty",
      "manpages/publish",
      "manpages/repl",
      "manpages/run dry-run",
      "manpages/run evaluate-call",
      "manpages/run evaluate-expr",
      "manpages/run interpret",
      "manpages/run test",
      "manpages/run test-expr",
      "manpages/transpile contract",
      "manpages/transpile-with-ast contract",
      "manpages/transpile-with-ast expression",
    ],
    "API": [
      "reference/toplevel",
      "reference/big-map-reference",
      "reference/bitwise-reference",
      "reference/bytes-reference",
      "reference/crypto-reference",
      "reference/list-reference",
      "reference/map-reference",
      "reference/set-reference",
      "reference/string-reference",
      "reference/option-reference",
      "reference/current-reference",
      "reference/test",
      "reference/proxy-ticket-reference",
    ],
    "Changelog":[
      "intro/changelog",
      "protocol/hangzhou",
      "protocol/ithaca",
      "protocol/jakarta",
      "protocol/kathmandu",
      "protocol/lima",
      "protocol/mumbai"
    ]
  },
  "faq": {
    "FAQ": [
      "faq/intro",
      "faq/convert-address-to-contract",
      "faq/polymorphic-comparison",
      "faq/catch-error-view",
      "faq/cameligo-ocaml-syntax-diff",
      "faq/tezos-now-advance-time",
      "faq/transpile-pascaligo-to-jsligo",
    ],
  },
};

module.exports = sidebars;
