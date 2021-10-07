import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeEntrypointAction, ChangeParametersAction, EvaluateFunctionState } from '../../redux/evaluate-function';
import { Group, Input, Label, Textarea } from '../form/inputs';

const Container = styled.div``;

export const EvaluateFunctionPaneComponent = (props: {theme: 'light' | 'dark'}) => {
  const isDark = props.theme === 'dark'
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
        <Label htmlFor="entrypoint">Function name</Label>
        <Input
          dark={isDark}
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
          dark={isDark}
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
