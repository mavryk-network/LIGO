import React, { FC } from 'react';
import { connect } from 'react-redux';
import styled, { css } from 'styled-components';

import { CommandType } from '../../redux/types';
import { Statusbar } from '../statusBar';
import CompileOutputPane from './compile-output-pane';
import DeployOutputPane from './deploy-output-pane';
import GenerateDeployScriptOutputPane from './generate-deploy-script-output-pane';
import { Loading } from './loading';
import OutputPane from './output-pane';

const Container = styled.div<{ visible?: boolean }>`
  z-index: 2;
  box-sizing: border-box;
  width: -webkit-fill-available;
  height: 100%;
  minHeight: "50px";
  overflow-x: hidden; 
  overflow-y: auto; 
  background-color: white;

  font-family: Menlo, Monaco, 'Courier New', monospace;
  display: flex;
  flex-direction: column;

 
  transition: transform 0.2s ease-in;

  ${props =>
    props.visible ?
    css`
      transform: translateX(0px);
    `
    : css`
    visibility: hidden;
    transform: translateX(0px);
    1px solid var(--blue_trans1);
  `}
`;

interface propTypes {
  theme: 'light' | 'dark'
  selected?: boolean;
  onCancel?: () => void;
}

interface stateTypes {
  hasError?: boolean;
  output?: string;
  loading?: boolean;
  command?: string;
}

const OutputTab: FC<propTypes & stateTypes> = (props) => {
  const { selected, onCancel, output, hasError, loading, command, theme } = props
  
  let visible = selected;

  if( loading || output ) {
    visible = true
  }

  const renderResult = () => {
    if (loading) {
      return <><Statusbar error={false} /><Loading theme={theme} onCancel={onCancel}></Loading></>;
      } else if (!output) {
      return <></>;
    } else if (command === CommandType.Compile) {
      return <CompileOutputPane theme={theme}></CompileOutputPane>;
    } else if (command === CommandType.Deploy) {
      return <><Statusbar error={hasError} /><DeployOutputPane theme={theme}></DeployOutputPane></>;
    } else if (command === CommandType.GenerateDeployScript) {
      return <GenerateDeployScriptOutputPane theme={theme}></GenerateDeployScriptOutputPane>;
    }

    return <><Statusbar error={hasError}/><OutputPane theme={theme}></OutputPane></>;
  };

  return <Container visible={visible}>{renderResult()}</Container>;
};

function mapStateToProps(state) {
  const { result, loading } = state
  return { 
    output: result.output,
    hasError: result.error,
    command: result.command,
    loading: loading.loading
   }
}

export default connect(mapStateToProps, null)(OutputTab)