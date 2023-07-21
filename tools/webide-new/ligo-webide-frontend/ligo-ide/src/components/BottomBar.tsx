import { forwardRef, useImperativeHandle, useState, Component as ReactComponent } from "react";
import CacheRoute from "react-router-cache-route";

import { useSelector, connect } from "react-redux";
import redux from "~/base-components/redux";

import { actions, TerminalButton } from "~/base-components/workspace";
import ProtocolSelector from "~/ligo-components/ligo-compiler/bottombar/ProtocolSelector";
import LigoVersion from "~/ligo-components/ligo-compiler/bottombar/LigoVersion";
import ToggleButton from "~/base-components/ui-components/inputs/ToggleButton";

/* eslint-disable */
// @ts-ignore
const mapStateToProps = function (state) {
  return {
    offlineMode: state.offline,
  };
};
/* eslint-enable */

class ToggleOfflineButtonC extends ReactComponent {
  render() {
    /* eslint-disable */
    // @ts-ignore
    const { offlineMode } = this.props;
    return (
      <ToggleButton
        // @ts-ignore
        selected={offlineMode}
        onChange={() => {
          redux.dispatch("TOGGLE_OFFLINE_STATUS", !offlineMode);
        }}
        offStateLabel="offline"
        onStateLabel="online"
      />
    );
    /* eslint-enable */
  }
}

const ToggleOfflineButton = connect(mapStateToProps)(ToggleOfflineButtonC);

const BottomBar = forwardRef((_, ref) => {
  /* eslint-disable */
  actions.bottomBarRef = ref;
  // @ts-ignore
  const network: string = useSelector((state) => state.network);
  // @ts-ignore
  const projects = useSelector((state) => state.projects);
  const selectedProject = projects.get("selected");
  const loaded = selectedProject?.get("loaded");
  /* eslint-enable */

  const [position, setPosition] = useState<[number, number]>([1, 1]);

  useImperativeHandle(ref, () => ({
    updatePosition(pos: [number, number]) {
      setPosition(pos);
    },
  }));

  let projectButtons;
  if (loaded) {
    /* eslint-disable */
    projectButtons = (
      // @ts-ignore
      <>
        <ToggleOfflineButton />
        <CacheRoute
          path={["/local/:project"]}
          render={() => (
            <div
              className="p-1"
              style={{ fontSize: "14px" }}
            >{`Ln ${position[0]}, Col ${position[1]}`}</div>
          )}
        />
        <CacheRoute path={["/local/:project"]} component={LigoVersion} />
        <CacheRoute path={["/local/:project"]} component={ProtocolSelector} />
        <CacheRoute path={["/local/:project"]} component={TerminalButton} />
      </>
    );
    /* eslint-enable */
  }

  return (
    <div className="border-top-1 d-flex flex-row">
      <div className="flex-1" />
      {projectButtons}
    </div>
  );
});

export default BottomBar;
