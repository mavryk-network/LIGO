import React, {FC} from 'react';
import { connect } from 'react-redux';
import styled, {css} from 'styled-components';

type props = {isDark?: boolean}

const Output = styled.div`
  flex: 1;
  padding: 0.5em;
  display: flex;
  overflow: auto;
`;

const Pre = styled.pre`
  margin: 0;
  width: -webkit-fill-available;
  white-space: break-spaces;
`; 

const Container = styled.div<props>`
  display: flex;
  flex-direction: column;
  flex: 1;
  ${props => props.isDark && css`
    background-color: rgb(25, 26, 27);
    color: white;
  `}

  & > * {
    ${(props:any) => props.isDark && css`
      background-color: rgb(25, 26, 27);
      color: white;
    `}
  }
`

interface stateTypes {
  theme: 'light' | 'dark'
  output?: string;
}

const OutputPane: FC<stateTypes> = (props) => {
  const { output, theme } = props
  const isDark = theme === 'dark'

  return (
    <Container isDark={isDark}>
      <Output id="output">
        <Pre>{output}</Pre>
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

export default connect(mapStateToProps, null)(OutputPane)