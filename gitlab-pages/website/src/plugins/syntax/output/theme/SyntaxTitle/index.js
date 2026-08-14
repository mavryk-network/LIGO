function _extends() { return _extends = Object.assign ? Object.assign.bind() : function (n) { for (var e = 1; e < arguments.length; e++) { var t = arguments[e]; for (var r in t) ({}).hasOwnProperty.call(t, r) && (n[r] = t[r]); } return n; }, _extends.apply(null, arguments); }
import Highlight, { defaultProps } from "prism-react-renderer";
import React, { useEffect, useState } from "react";

// THE PROBLEM IS USE THEME CONTEXT ==>>>>
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { SyntaxContext } from "@theme/Syntax";
import defaultTheme from "prism-react-renderer/themes/palenight";
const {
  Prism
} = require("prism-react-renderer");
Prism.languages = {
  ...Prism.languages,
  cameligo: {
    ...Prism.languages.ocaml,
    comment: [/(^|[^\\])\/\*[\s\S]*?\*\//, /\(\*[\s\S]*?\*\)/, /\/\/.*/]
  },
  jsligo: Prism.languages.typescript,
  // MAVRYK: PascaLIGO — Prism grammar for the restored third syntax (Pascal-based).
  pascaligo: {
    comment: [/\(\*[\s\S]+?\*\)/, /\/\/.*/],
    string: {
      pattern: /(?:'(?:''|[^'\r\n])*'|#[&$%]?[a-f\d]+)+|\^[a-z]/i,
      greedy: true
    },
    keyword: [{
      pattern: /(^|[^&])\b(?:absolute|array|asm|begin|case|const|constructor|destructor|do|downto|else|end|file|for|function|goto|if|implementation|inherited|inline|interface|label|nil|object|of|operator|packed|procedure|program|record|reintroduce|repeat|self|set|string|then|to|type|unit|until|uses|var|while|with)\b/i,
      lookbehind: true
    }, {
      pattern: /(^|[^&])\b(?:dispose|exit|false|new|true)\b/i,
      lookbehind: true
    }, {
      pattern: /(^|[^&])\b(?:class|dispinterface|except|exports|finalization|finally|initialization|inline|library|on|out|packed|property|raise|resourcestring|threadvar|try)\b/i,
      lookbehind: true
    }, {
      pattern: /(^|[^&])\b(?:absolute|abstract|alias|assembler|bitpacked|break|cdecl|continue|cppdecl|cvar|default|deprecated|dynamic|enumerator|experimental|export|external|far|far16|forward|generic|helper|implements|index|interrupt|iochecks|local|message|name|near|nodefault|noreturn|nostackframe|oldfpccall|otherwise|overload|override|pascal|platform|private|protected|public|published|read|register|reintroduce|result|safecall|saveregisters|softfloat|specialize|static|stdcall|stored|strict|unaligned|unimplemented|varargs|virtual|write)\b/i,
      lookbehind: true
    }],
    number: [/(?:[&%]\d+|\$[a-f\d]+)/i, /\b\d+(?:\.\d+)?(?:e[+-]?\d+)?/i],
    operator: [/\.\.|\*\*|:=|<[<=>]?|>[>=]?|[+\-*\/]=?|[@^=]/i, {
      pattern: /(^|[^&])\b(?:and|as|div|exclude|in|include|is|mod|not|or|shl|shr|xor)\b/,
      lookbehind: true
    }],
    punctuation: /\(\.|\.\)|[()\[\]:;,.]/
  }
};
function SyntaxTitle(props) {
  const {
    siteConfig: {
      themeConfig: {
        prism = {}
      }
    }
  } = useDocusaurusContext();
  const lightModeTheme = prism.singleTheme || defaultTheme;
  // todo fix Hook is called outside the <ColorModeProvider>. Please see https://docusaurus.io/docs/api/themes/configuration#use-color-mode.
  // const {colorMode, setColorMode} = useColorMode();
  // const prismTheme = colorMode === "dark" ? darkModeTheme : lightModeTheme;

  const [mounted, setMounted] = useState(false);
  useEffect(() => {
    setMounted(true);
  }, []);
  return /*#__PURE__*/React.createElement(SyntaxContext.Consumer, null, ({
    syntax
  }) => {
    if (syntax === props.syntax) {
      return /*#__PURE__*/React.createElement(Highlight, _extends({}, defaultProps, {
        key: mounted,
        language: props.syntax,
        code: props.children,
        theme: lightModeTheme
      }), ({
        className,
        tokens,
        getLineProps,
        getTokenProps
      }) => /*#__PURE__*/React.createElement("h4", {
        className: className,
        style: {
          backgroundColor: "var(--ifm-background-color)",
          fontFamily: "var(--ifm-font-family-monospace)",
          lineHeight: "var(--ifm-pre-line-height)",
          fontSize: "1.1rem",
          fontWeight: "bold",
          padding: 0,
          whiteSpace: "break-spaces",
          marginTop: "3rem"
        }
      }, tokens.map((line, i) => /*#__PURE__*/React.createElement("div", getLineProps({
        line,
        key: i
      }), line.map((token, key) => /*#__PURE__*/React.createElement("span", getTokenProps({
        token,
        key
      })))))));
    } else {
      return /*#__PURE__*/React.createElement("div", null);
    }
  });
}
export default SyntaxTitle;
