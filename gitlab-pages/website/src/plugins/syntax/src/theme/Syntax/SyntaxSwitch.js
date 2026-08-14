import { useHistory } from "@docusaurus/router";
import React, { useCallback } from "react";

import styles from "./styles.module.css";

// MAVRYK: PascaLIGO. Restored the 3-way syntax selector (JsLIGO / CameLIGO / PascaLIGO).
// The previous binary toggle could only ever represent two syntaxes; a <select> scales to
// three. The `.syntaxSwitch` styles it consumes already live in styles.module.css (0.60-era).
function SyntaxSwitch(props) {
  const history = useHistory();

  const onSyntaxChange = useCallback(
    (value) => {
      if (typeof window === "undefined") return;
      history.replace({
        search: `?lang=${value}`,
      });
      localStorage.setItem("syntax", value);
      props.onSyntaxChange(value);
    },
    [props]
  );

  return (
    <form>
      <select
        className={styles.syntaxSwitch}
        value={props.syntax}
        aria-label={`preferred syntax: ${props.syntax}`}
        onChange={(e) => onSyntaxChange(e.target.value)}
      >
        <option value="jsligo">JsLIGO</option>
        <option value="cameligo">CameLIGO</option>
        <option value="pascaligo">PascaLIGO</option>
      </select>
    </form>
  );
}

export default SyntaxSwitch;
