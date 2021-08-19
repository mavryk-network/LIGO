import React, { useReducer, useEffect } from "react";
import JoyRide, { ACTIONS, EVENTS, STATUS } from "react-joyride";

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
    target: '.configure',
    title: 'Configure',
    content: 'Select your action: Evaluate Function, Compile, Compile Function, Deploy, Dry Run, Evaluate Value, Generate Deploy Script.',
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
    title: '?',
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
  run: false,
  continuous: true,
  loading: false,
  stepIndex: 0,
  steps: TOUR_STEPS,
};

// Set up the reducer function
const reducer = (state = INITIAL_STATE, action) => {
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
};

// Define the Tour component
const Tour = () => {
  const [tourState, dispatch] = useReducer(reducer, INITIAL_STATE);
  useEffect(() => {
    if (!localStorage.getItem("tour")) {
      dispatch({ type: "START" });
    }
  }, []);
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
