/**
// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  "docs": {
    "Getting started": [
      "intro/introduction",
      {
        "type": "category",
        "label": "Installation",
        "items": [
          "intro/installation",
          "intro/editor-support",
        ],
      },
      "tutorials/getting-started/getting-started",
    ],
    "Writing a Contract": [
      {
        "type": "category",
        "label": "First contract",
        "items": [
          "tutorials/taco-shop/mavryk-taco-shop-smart-contract",
          "tutorials/taco-shop/mavryk-taco-shop-payout",
        ],
      },
      "tutorials/start-a-project-from-a-template",
    ],
    "Syntax": [
      "comments/comments",
      "syntax/variables",
      {
        "type": "category",
        "label": "Functions",
        "items": [
          "functions/declaring",
          "functions/lambdas",
          "functions/higher-order",
          "functions/inlining",
          "functions/recursion",
        ],
      },
      {
        "type": "category",
        "label": "Flow control",
        "items": [
          "imperative/looping",
          "imperative/failing",
          "imperative/asserting",
          "imperative/switches",
        ],
      },
      {
        "type": "category",
        "label": "Modules/Namespaces",
        "items": [
          "modules/declaring",
          "modules/accessing",
          "modules/nesting",
          "modules/aliasing",
          "modules/importing",
          "modules/including",
        ],
      },
      {
        "type": "category",
        "label": "Attributes/Decorators",
        "items": [
          "mavryk/decorators/decorators",
          "mavryk/decorators/annot",
          "mavryk/decorators/deprecated",
          "mavryk/decorators/dyn_entry",
          "mavryk/decorators/entry",
          "mavryk/decorators/inline",
          "mavryk/decorators/layout",
          "mavryk/decorators/private",
          "mavryk/decorators/view",
        ],
      },
      {
        "type": "category",
        "label": "Contracts",
        "items": [
          "mavryk/contracts/contracts",
          "mavryk/contracts/contract-address",
          "mavryk/contracts/contract_of",
          "mavryk/contracts/michelson",
          "mavryk/contracts/michelson-injection",
          "mavryk/contracts/operation",
          "contract/events",
        ],
      },
      {
        "type": "category",
        "label": "Signatures/Interfaces",
        "items": [
          "signatures/declaring",
          "signatures/extending",
        ],
      },
      {
        "type": "category",
        "label": "Keywords",
        "items": [
          "keywords/keywords",
          "keywords/escaped_vars",
        ],
      },
    ],
    "Data types": [
      {
        "type": "category",
        "label": "Primitive types",
        "items": [
          "data-types/numbers",
          "data-types/booleans",
          "data-types/strings",
          "data-types/timestamp",
          "data-types/bytes",
        ],
      },
      {
        "type": "category",
        "label": "Complex types",
        "items": [
          "data-types/tuples",
          "data-types/variants",
          "data-types/lists",
          "data-types/records",
          "data-types/sets",
          "data-types/maps",
        ],
      },
      {
        "type": "category",
        "label": "Mavryk-specific types",
        "items": [
          "mavryk/mav",
          "mavryk/key",
          "mavryk/hash_key",
          "mavryk/signature",
          "mavryk/addresses",
          "mavryk/contracts-type",
          {
            "type": "category",
            "label": "Big sets",
            "items": [
              "mavryk/big_sets/declaring",
              "mavryk/big_sets/searching",
              "mavryk/big_sets/adding",
              "mavryk/big_sets/removing",
              "mavryk/big_sets/updating",
            ],
          },
          {
            "type": "category",
            "label": "Big maps",
            "items": [
              "mavryk/big_maps/declaring",
              "mavryk/big_maps/searching",
              "mavryk/big_maps/adding",
              "mavryk/big_maps/removing",
              "mavryk/big_maps/updating",
            ],
          },
        ],
      },
      {
        "type": "category",
        "label": "Polymorphism",
        "items": [
          "polymorphism/polymorphism",
          "polymorphism/parametric_types",
          "polymorphism/functions",
        ],
      },
    ],
    "Preprocessor": [
      "preprocessor/preprocessor",
      "preprocessor/comments",
      "preprocessor/strings",
      "preprocessor/if",
      "preprocessor/define",
      "preprocessor/include",
      "preprocessor/import",
      "preprocessor/error",
    ],
    "Testing": [
      "testing/testing",
      "testing/testing-tickets",
      "advanced/mutation-testing",
      "advanced/michelson_testing",
    ],
    "Advanced Topics": [
      "advanced/package-management",
      "tutorials/optimisation/optimisation",
      "tutorials/security/security",
    ]
  },
  "API": {
    "Language": [
      "reference/bytes-reference",
      "reference/toplevel-reference",
      "reference/tuple2-reference",
      "reference/set-reference",
      "reference/option-reference",
      "reference/dynamic-entrypoints-reference",
      "reference/map-reference",
      "reference/big-set-reference",
      "reference/string-reference",
      "reference/big-map-reference",
      "reference/bitwise-reference",
      "reference/list-reference",
      "reference/crypto-reference",
      {
        "type": "category",
        "label": "mavryk",
        "items": [
          "reference/mavryk-reference",
          {
            "type": "category",
            "label": "next",
            "items": [
              "reference/mavryk.next.ticket-reference",
              "reference/mavryk.next-reference",
              "reference/mavryk.next.sapling-reference",
              "reference/mavryk.next.view-reference",
              "reference/mavryk.next.operation-reference"
            ]
          }
        ]
      },
      {
        "type": "category",
        "label": "test",
        "items": [
          "reference/test.pbt-reference",
          "reference/test-reference",
          "reference/test.proxy-ticket-reference",
          {
            "type": "category",
            "label": "next",
            "items": [
              "reference/test.next.compare-reference",
              "reference/test.next.contract-reference",
              "reference/test.next.ticket-reference",
              "reference/test.next.crypto-reference",
              "reference/test.next.address-reference",
              "reference/test.next.typed-address-reference",
              "reference/test.next-reference",
              "reference/test.next.io-reference",
              "reference/test.next.string-reference",
              "reference/test.next.dynamic-entrypoints-reference",
              "reference/test.next.timelock-reference",
              "reference/test.next.originate-reference",
              {
                "type": "category",
                "label": "michelson",
                "items": [
                  "reference/test.next.michelson-reference",
                  "reference/test.next.michelson.contract-reference"
                ]
              },
              {
                "type": "category",
                "label": "account",
                "items": [
                  "reference/test.next.account-reference",
                  "reference/test.next.account.contract-reference"
                ]
              },
              {
                "type": "category",
                "label": "mutation",
                "items": [
                  "reference/test.next.mutation.all-reference",
                  "reference/test.next.mutation-reference"
                ]
              },
              {
                "type": "category",
                "label": "assert",
                "items": [
                  "reference/test.next.assert.error-reference",
                  "reference/test.next.assert-reference"
                ]
              },
              {
                "type": "category",
                "label": "state",
                "items": [
                  "reference/test.next.state-reference",
                  "reference/test.next.state.reset-reference"
                ]
              }
            ]
          }
        ]
      },
      {
        "type": "category",
        "label": "assert",
        "items": [
          "reference/assert.error-reference",
          "reference/assert-reference"
        ]
      }
    ],
    "CLI": [
      {
        "type": "doc",
        "id": "manpages/ligo"
      },
      {
        "type": "category",
        "label": "ligo compile",
        "items": [
          "manpages/compile constant",
          "manpages/compile contract",
          "manpages/compile expression",
          "manpages/compile parameter",
          "manpages/compile storage"
        ]
      },
      {
        "type": "category",
        "label": "ligo run",
        "items": [
          "manpages/run dry-run",
          "manpages/run evaluate-call",
          "manpages/run evaluate-expr",
          "manpages/run interpret",
          "manpages/run test",
          "manpages/run test-expr"
        ]
      },
      {
        "type": "category",
        "label": "ligo print",
        "items": [
          "manpages/print ast-aggregated",
          "manpages/print ast-core",
          "manpages/print ast-typed",
          "manpages/print ast-expanded",
          "manpages/print ast-unified",
          "manpages/print cst",
          "manpages/print dependency-graph",
          "manpages/print mini-c",
          "manpages/print preprocessed",
          "manpages/print pretty"
        ]
      },
      {
        "type": "category",
        "label": "ligo transpile",
        "items": [
          "manpages/transpile contract",
          "manpages/transpile-with-ast contract",
          "manpages/transpile-with-ast expression"
        ]
      },
      {
        "type": "category",
        "label": "ligo info",
        "items": [
          "manpages/info get-scope",
          "manpages/info list-declarations",
          "manpages/info measure-contract"
        ]
      },
      {
        "type": "category",
        "label": "ligo analytics",
        "items": [
          "manpages/analytics accept",
          "manpages/analytics deny"
        ]
      },
      {
        "type": "category",
        "label": "ligo init",
        "items": [
          "manpages/init contract",
          "manpages/init library"
        ]
      },
      {
        "type": "doc",
        "label": "ligo changelog",
        "id": "manpages/changelog"
      },
      {
        "type": "doc",
        "label": "ligo install",
        "id": "manpages/install"
      },
      {
        "type": "category",
        "label": "ligo registry",
        "items": [
          "manpages/registry add-user",
          "manpages/registry login",
          "manpages/registry publish",
          "manpages/registry unpublish"
        ]
      },
      {
        "type": "doc",
        "label": "ligo repl",
        "id": "manpages/repl"
      }
    ],
    "Changelog": [
      "intro/changelog",
      "protocol/atlas"
    ]
  },
  "faq": {
    "FAQ": [
      "faq/intro",
      "faq/convert-address-to-contract",
      "faq/polymorphic-comparison",
      "faq/catch-error-view",
      "faq/cameligo-ocaml-syntax-diff",
      "faq/mavryk-now-advance-time"
    ]
  }
};

module.exports = sidebars;
