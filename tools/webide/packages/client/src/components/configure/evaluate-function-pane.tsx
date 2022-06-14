import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { protocolType } from '../../redux/compile';
import { ChangeEntrypointAction, ChangeParametersAction, ChangeProtocolAction, EvaluateFunctionState } from '../../redux/evaluate-function';
import { Group, Input, Label, Textarea } from '../form/inputs';
import { Option, SelectCommand } from '../form/select';

const Container = styled.div``;

export const EvaluateFunctionPaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, EvaluateFunctionState['entrypoint']>(
    state => state.evaluateFunction && state.evaluateFunction.entrypoint
  );
  const parameters = useSelector<AppState, EvaluateFunctionState['parameters']>(
    state => state.evaluateFunction && state.evaluateFunction.parameters
  );

  return (
    <Container>
      <Group>
        <Label htmlFor="protocol">Choose a protocol (used for compilation)</Label>
        <SelectCommand
          id="protocol-select"
          value={protocolType.Ithaca}
          onChange={ev =>
            dispatch({ ...new ChangeProtocolAction(ev.target.value) })
          }>
          <Option value={protocolType.Ithaca}>Ithaca</Option>
        </SelectCommand>
        <Label htmlFor="entrypoint">Function name</Label>
        <Input
          id="entrypoint"
          value={entrypoint}
          onChange={ev =>
            dispatch({ ...new ChangeEntrypointAction(ev.target.value) })
          }
        ></Input>
      </Group>
      <Group>
        <Label htmlFor="parameters">Parameters</Label>
        <Textarea
          id="parameters"
          rows={9}
          value={parameters}
          onChange={ev =>
            dispatch({ ...new ChangeParametersAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
    </Container>
  );
};
