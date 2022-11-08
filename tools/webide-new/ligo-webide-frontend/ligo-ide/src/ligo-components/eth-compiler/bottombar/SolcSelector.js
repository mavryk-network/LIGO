import React from "react";

import { DropdownItem } from "~/base-components/ui-components";

import notification from "~/base-components/notification";
import { DockerImageSelector } from "~/base-components/docker";
import { ProjectManager } from "~/base-components/workspace";

let n;

export default function (props) {
  const [hasDefault, setHasDefault] = React.useState(!props.remote);
  const [selected, onSelected] = React.useState("");

  React.useEffect(
    ProjectManager.effect("settings:framework", (framework) => {
      if (props.remote) {
        setHasDefault(false);
        return;
      }
      setHasDefault(framework.endsWith("-docker"));
    }),
    []
  );

  React.useEffect(
    ProjectManager.effect("settings:compilers.solc", (v) => {
      // if (!props.remote) {
      //   if (v === 'default') {
      //     n = notification.info('Solc from truffle-config.js Selected', 'The version of solc used in compilation will be determined by <b>truffle-config.js</b>.', 4)
      //   } else if (v) {
      //     n = notification.info(`Solc v${v} Selected`, `This will overwrite the configuration of <b>truffle-config.js</b> in compilation.`, 4)
      //   }
      // }
      onSelected(v);
    }),
    []
  );

  return (
    <DockerImageSelector
      channel={props.solc}
      disableAutoSelection
      size="sm"
      icon="fas fa-hammer"
      title="Solc"
      noManager
      selected={selected}
      selectedText={selected === "default" ? "truffle-config.js" : undefined}
      onSelected={(v) => ProjectManager.instance.projectSettings?.set("compilers.solc", v)}
    >
      {hasDefault && (
        <>
          <DropdownItem
            active={selected === "default"}
            onClick={() =>
              ProjectManager.instance.projectSettings?.set("compilers.solc", "default")
            }
          >
            From truffle-config.js
          </DropdownItem>
          <DropdownItem divider />
        </>
      )}
    </DockerImageSelector>
  );
}
