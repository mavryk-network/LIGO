import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum ActionType {
  ChangeEntrypoint = 'evaluate-function-change-entrypoint',
  ChangeParameters = 'evaluate-function-change-parameters'
}

export interface EvaluateFunctionState {
  entrypoint: string;
  parameters: string;
  protocol: string;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: EvaluateFunctionState['entrypoint']) {}
}

export class ChangeParametersAction {
  public readonly type = ActionType.ChangeParameters;
  constructor(public payload: EvaluateFunctionState['parameters']) {}
}

export class ChangeProtocolAction {
  public readonly type = ActionType.ChangeParameters;
  constructor(public payload: EvaluateFunctionState['protocol']) {}
}

type Action =
  | ChangeEntrypointAction
  | ChangeParametersAction
  | ChangeProtocolAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: EvaluateFunctionState = {
  entrypoint: '',
  parameters: '',
  protocol: 'ithaca'
};

const evaluateFunction = (
  state = DEFAULT_STATE,
  action: Action
): EvaluateFunctionState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.evaluateFunction)
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
    case ActionType.ChangeParameters:
      return {
        ...state,
        parameters: action.payload
      };
    default:
      return state;
  }
};

export default evaluateFunction
