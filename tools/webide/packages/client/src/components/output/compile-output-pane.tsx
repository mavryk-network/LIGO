import React, { useRef } from 'react';
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
  padding-bottom: 20px;
`;

const Container = styled.div<{ visible?: boolean, isDark?: boolean}>`
  display: flex;
  flex-direction: column;
  height: 100%;

  & * {
    ${props => props.isDark && css`
      background-color: rgb(25, 26, 27);
      color: white !important;
  `}
  }
`;


const CompileOutputPane = (props: {theme: 'light' | 'dark', output?: string}) => {
  // var parse = require('shell-quote').parse;
  const { output, theme } = props
  const isDark = theme === 'dark'


  const preRef = useRef<HTMLPreElement>(null);

  return (
    <Container isDark>
      <OutputToolbarComponent
        theme={theme}
        showTryMichelson={true}
        onCopy={() => copyOutput(preRef.current)}
        onDownload={() => downloadOutput(output || '')}
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

export default connect(mapStateToProps, null)(CompileOutputPane)
