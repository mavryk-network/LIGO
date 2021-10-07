import React, { useRef, FC } from 'react';
import { connect } from 'react-redux';
import styled, {css} from 'styled-components';

import OutputToolbarComponent from './output-toolbar';
import { copyOutput, downloadOutput } from './utils';

const Output = styled.div`
  flex: 1;
  padding: 0.5em;
  display: flex;
  overflow: hidden;
`;

const Pre = styled.pre`
  margin: 0;
  width: -webkit-fill-available;
  white-space: break-spaces;
`;

const Container = styled.div<{ visible?: boolean, isDark?: boolean }>`
  display: flex;
  flex-direction: column;
  height: 100%;

  & div, & pre {
    ${props => props.isDark && css`
      background-color: rgb(25, 26, 27);
      color: white;
  `}
  }
`;

interface stateTypes {
  theme: 'light' | 'dark',
  output?: string;
}

const GenerateDeployScriptOutputPane: FC<stateTypes> = (props) => {
  
  const {output, theme} = props
  const downloadResult = output ? output : ''
  const isDark = theme === 'dark'
  const preRef = useRef<HTMLPreElement>(null);

  return (
    <Container isDark>
      <OutputToolbarComponent
        onCopy={() => copyOutput(preRef.current)}
        onDownload={() => downloadOutput(downloadResult)}
        theme={theme}
      ></OutputToolbarComponent>
      <Output id="output">
        <Pre ref={preRef}>{output}</Pre>
      </Output>
    </Container>
  );
};

function mapStateToProps(state) {
  const { result } = state
  return { 
    output: result.output,
  }
}

export default connect(mapStateToProps, null)(GenerateDeployScriptOutputPane)
