import React from "react";
import Highlight, { defaultProps } from "prism-react-renderer";
import Tabs from "@theme/Tabs";
import TabItem from "@theme/TabItem";
import { useColorMode } from "@docusaurus/theme-common";
import defaultTheme from "prism-react-renderer/themes/palenight";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";

import CAMELIGO_EXAMPLE from '!!raw-loader!./cameligo.mligo';
import JSLIGO_EXAMPLE from '!!raw-loader!./jsligo.jsligo';
import CAMELIGO_FULL_EXAMPLE from '!!raw-loader!./cameligo-full.mligo';
import JSLIGO_FULL_EXAMPLE from '!!raw-loader!./jsligo-full.jsligo';
function CodeExamples(props) {
  const {
    siteConfig: {
      themeConfig: { prism = {} },
    },
  } = useDocusaurusContext();
  const lightModeTheme = prism.theme || defaultTheme;
  const darkModeTheme = prism.darkTheme || lightModeTheme;
  const prismTheme =
    useColorMode().colorMode === "dark" ? darkModeTheme : lightModeTheme;

  return (
    <Tabs
      defaultValue="jsligo"
      values={[
        { label: "JsLIGO", value: "jsligo" },
        { label: "CameLIGO", value: "cameligo" },
        { label: "JsLIGO full", value: "jsligo-full" },
        { label: "CameLIGO full", value: "cameligo-full" }
      ]}
    >
      <TabItem value="jsligo">
        <Highlight
          {...defaultProps}
          language="jsligo"
          code={JSLIGO_EXAMPLE}
          theme={prismTheme}
        >
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={className} style={style}>
              {tokens.map((line, i) => (
                <div {...getLineProps({ line, key: i })}>
                  {line.map((token, key) => (
                    <span {...getTokenProps({ token, key })} />
                  ))}
                </div>
              ))}
            </pre>
          )}
        </Highlight>
      </TabItem>

      <TabItem value="cameligo">
        <Highlight
          {...defaultProps}
          language="cameligo"
          code={CAMELIGO_EXAMPLE}
          theme={prismTheme}
        >
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={className} style={style}>
              {tokens.map((line, i) => (
                <div {...getLineProps({ line, key: i })}>
                  {line.map((token, key) => (
                    <span {...getTokenProps({ token, key })} />
                  ))}
                </div>
              ))}
            </pre>
          )}
        </Highlight>
      </TabItem>

      <TabItem value="cameligo-full">
        <Highlight
          {...defaultProps}
          language="cameligo"
          code={CAMELIGO_FULL_EXAMPLE}
          theme={prismTheme}
        >
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={className} style={style}>
              {tokens.map((line, i) => (
                <div {...getLineProps({ line, key: i })}>
                  {line.map((token, key) => (
                    <span {...getTokenProps({ token, key })} />
                  ))}
                </div>
              ))}
            </pre>
          )}
        </Highlight>
      </TabItem>

      <TabItem value="jsligo-full">
        <Highlight
          {...defaultProps}
          language="jsligo"
          code={JSLIGO_FULL_EXAMPLE}
          theme={prismTheme}
        >
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={className} style={style}>
              {tokens.map((line, i) => (
                <div {...getLineProps({ line, key: i })}>
                  {line.map((token, key) => (
                    <span {...getTokenProps({ token, key })} />
                  ))}
                </div>
              ))}
            </pre>
          )}
        </Highlight>
      </TabItem>
    </Tabs >

  );
}

export default function HomepageCodeExamples() {
  return (
    <div id="preview">
      <CodeExamples />
    </div>
  );
}
