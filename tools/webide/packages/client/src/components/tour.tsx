import React, { useReducer } from "react";
import JoyRide, { ACTIONS, EVENTS, STATUS } from "react-joyride";
import styled from 'styled-components'

const ActionHelp = styled.div`
  font-size: smaller;
  color: slategray;
  line-height: 0.98;
  margin-top: 5px;
  margin-left: 5px;
  margin-bottom: 10px;
`

const TOUR_STEPS = [
  {
    target: '.editor',
    title: 'Smart Contract Editor',
    content: 'When you write in LIGO, you’re writing high level code that’s easy to learn',
    disableBeacon: true
  },
  {
    target: '.synyax-select',
    title: 'Syntax Selector',
    content: 'Choose a familiar LIGO syntax based on what you already know',
    disableBeacon: true
  },{
    target: '.examples',
    title: 'Examples',
    content: 'Not sure what to write? Check out our example contracts',
    disableBeacon: true
  },{
    target: '#command-select',
    title: 'Actions',
    content: (
      <>
        <ul>
          <li>
            Compile Contract
            <ActionHelp>Compile your LIGO coded contract to Michelson</ActionHelp>
          </li>
          <li>
            Evaluate Value
            <ActionHelp>Evaluate and print the last expression in your source code</ActionHelp>
          </li>
          <li>
            Evalute Function
            <ActionHelp>Call and evaluate a function in your LIGO contract with the given input</ActionHelp>
          </li>
          <li>
            Dry Run
            <ActionHelp>Run the smart contract with the given storage and input</ActionHelp>
          </li>
          <li>
            Generate Deployment Script
            <ActionHelp>Generate a script to deploy your smart contract to a network</ActionHelp>
          </li>
          <li>
            Deploy
            <ActionHelp>Deploy the smart contract to a Tezos network</ActionHelp>
          </li>
        </ul>
      </>
    ),
    disableBeacon: true
  },{
    target: '.entrypoint',
    title: 'Configuration',
    content: 'Input the configuration parameters associated with the selected action.',
    disableBeacon: true
  },{
    target: '.editor-title',
    title: 'Contract Name',
    content: 'Editable name of the contract.',
    disableBeacon: true
  },{
    target: '.share-link',
    title: 'Share Code',
    content: 'Share your code with others! Copies link to the code into the clipboard.',
    disableBeacon: true
  },{
    target: '.docs',
    title: 'Documentation',
    content: 'Detailed documentation from installation and language basics to advanced concepts and cli commands.',
    disableBeacon: true
  },{
    target: '.tutorial',
    title: 'Tutorials',
    content: 'Learn how to set up and interact with the LIGO CLI and get started with a Taco Shop smart contract. Help the owner of Taco Shop, Pedro, receive or donate his profits.',
    disableBeacon: true
  },{
    target: '.cheatSheet',
    title: 'Cheat Sheet',
    content: 'Check out our Cheatsheet for coding help',
    disableBeacon: true
  },
  {
    target: '.askQuestions',
    title: 'Ask Questions',
    content: 'Ask us questions and follow us on other social media platforms.',
    disableBeacon: true
  },{
    target: '.question',
    title: "We're here to help",
    content: 'If you get stuck, don’t hesitate to ask us questions. We’re here to help',
    disableBeacon: true
  },{
    target: '.report',
    title: 'Report an issue',
    content: 'Create an issue on GitLab and we will get back to you.',
    disableBeacon: true
  }
  ]

// Define our state
const INITIAL_STATE = {
  key: new Date(),
  run: true,
  continuous: true,
  loading: false,
  stepIndex: 0,
  steps: TOUR_STEPS,
};

const getInitialState = () => {
  const retval = localStorage.getItem('tour')
  return retval ? JSON.parse(retval) : INITIAL_STATE
}

// Set up the reducer function
const reducer = (state = getInitialState(), action) => {
  const retval = (function(){
    switch (action.type) {
      case "START":
        return { ...state, run: true };
      case "RESET":
        return { ...state, stepIndex: 0 };
      case "STOP":
        return { ...state, run: false };
      case "NEXT_OR_PREV":
        return { ...state, ...action.payload };
      case "RESTART":
        return {
          ...state,
          stepIndex: 0,
          run: true,
          loading: false,
          key: new Date(),
        };
      default:
        return state;
    }
  })()

  localStorage.setItem('tour', JSON.stringify(retval))

  return retval
};

// Define the Tour component
const Tour = () => {
  const [tourState, dispatch] = useReducer(reducer, getInitialState());

  const callback = (data) => {
    const { action, index, type, status } = data;
    if (
      action === ACTIONS.CLOSE ||
      (status === STATUS.SKIPPED && tourState.run) ||
      status === STATUS.FINISHED
    ) {
      dispatch({ type: "STOP" });
    } else if (type === EVENTS.STEP_AFTER || type === EVENTS.TARGET_NOT_FOUND) {
      dispatch({
        type: "NEXT_OR_PREV",
        payload: { stepIndex: index + (action === ACTIONS.PREV ? -1 : 1) },
      });
    }
  };
  const startTour = () => {
    dispatch({ type: "RESTART" });
  };
  return (
    <>
      <button className="btn btn-primary" onClick={startTour}>
        Start Tour
      </button>
      <JoyRide
        {...tourState}
        autoStart
        showStepsProgress
        callback={callback}
        showSkipButton={true}
        styles={{
          tooltipContainer: {
            textAlign: "left",
          },
          buttonNext: {
            backgroundColor: "#057cfe"
          },
          buttonBack: {
            marginRight: 10,
            backgroundColor: "#057cfe",
            color: 'white'
          },
        }}
        locale={{
          last: "End tour",
        }}
      />
    </>
  );
};
export default Tour;
