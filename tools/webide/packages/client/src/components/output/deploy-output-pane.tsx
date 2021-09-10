import React from 'react';
import { connect } from 'react-redux';
import styled, {css} from 'styled-components';

const Output = styled.div`
  flex: 1;
  padding: 0.5em 0.5em 0.5em 0.5em;
  display: flex;
  flex-direction: column;
`;

const Pre = styled.pre<{isDark?: boolean}>`
  padding: 0.5em;
  margin: 0 -0.5em;
  overflow: hidden;
  height: 100%;
  width: -webkit-fill-available;
  white-space: normal;
`;

const Container = styled.div<{ visible?: boolean, isDark?: boolean }>`
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: auto;

  & div, & pre {
    ${props => props.isDark && css`
      background-color: rgb(25, 26, 27);
      color: white;
  `}
  }
`;

const DeployOutputPane = (props: {theme: 'light' | 'dark', contract?: string, output?: string, network?: string}) => {
const {contract, output, network, theme} = props
const isDark = theme === 'dark'
let networkUrlPart = network
  return (
    <Container isDark>
      <Output id="output">
        {contract && (
          <div>
            The contract was successfully deployed to the {network} test network.
            <br />
            <br />
            View your new contract using{' '}
            <a
              target="_blank"
              rel="noopener noreferrer"
              href={`https://better-call.dev/${networkUrlPart}/${contract}`}
            >
              Better Call Dev
            </a>
            !
            <br />
            <br />
            <b>The address of your new contract is: </b>
            <i>{contract}</i>
            <br />
            <br />
            <b>The initial storage of your contract is: </b>
          </div>
        )}
        {output && <Pre>{output}</Pre>}
      </Output>
    </Container>
  );
};

function mapStateToProps(state) {
  const { result, deploy } = state
  return { 
    output: result.output,
    contract: result.contract,
    network: deploy.network
   }
}

export default connect(mapStateToProps, null)(DeployOutputPane)
