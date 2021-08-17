import React, { FC } from "react";
import { useTray } from "./tray-context";
import styled from 'styled-components'
import { SpacedBox } from "./space-box";
import Subtitle from "./subTitle";
import Icon from "./icon";


const sizes = {
  lg: "700px",
  large: "700px",
  sm: "350px",
  small: "350px",
};

const TrayComponent = styled.div<{size: Size}>`
  position: absolute;
  display: flex;
  align-items: center;
  flex-direction: column;
  width: ${(props) => sizes[props.size]};
  max-width: 90vw;
  overflow-y: auto;
  height: 100vh;
  top: 0;
  right: 0;
  background: white;
  z-index: 3;

  @media (max-width: 480px) {
    width: 90%;
  }
`;

const CloseButton = styled.div`
  background-color: white;
  z-index: 1;
  margin: 3px 0 0 5px;
  display: flex;
  align-self: flex-start;
`;

const TrayBody = styled.div`
  width: 100%;
  height: 100%;
  padding: 0 15px 15px 15px;
  display: flex;
  flex-direction: column;
`;

const ModalBackdrop = styled.div<{size: Size}>`
  background: #000;
  opacity: 0.5;
  width: calc(100% - ${(props) => sizes[props.size]});
  min-width: 10vw;
  height: 100%;
  position: absolute;
  display: flex;
  z-index: 3;
  @media (max-width: 480px) {
    width: 10%;
  }
`;

type Size = "lg" | "sm" | "large" | "small"


const Tray: FC<{}> = () => {
  const { isTrayOpen, traySize, closeTray, trayContent, trayTitle } = useTray();

  if (!isTrayOpen) {
    return null;
  }

  return (
    <>
      <ModalBackdrop size={traySize} onClick={closeTray} />

      <TrayComponent size={traySize}>
        <SpacedBox>
          <CloseButton onClick={closeTray}>
            <Icon.X color="#007bff" />
          </CloseButton>

          <Subtitle>{trayTitle}</Subtitle>
        </SpacedBox>

        <TrayBody>{trayContent}</TrayBody>
      </TrayComponent>
    </>
  );
};

export default Tray;
