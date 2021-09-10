import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeEntrypointAction, ChangeParametersAction, ChangeStorageAction, DryRunState } from '../../redux/dry-run';
import { AccessFunctionLabel, Group, Input, Label, Textarea } from '../form/inputs';

const Container = styled.div``;

export const DryRunPaneComponent = (props: {theme: 'light' | 'dark'}) => {
  const isDark = props.theme === 'dark';
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, DryRunState['entrypoint']>(
    state => state.dryRun && state.dryRun.entrypoint
  );
  const parameters = useSelector<AppState, DryRunState['parameters']>(
    state => state.dryRun && state.dryRun.parameters
  );
  const storage = useSelector<AppState, DryRunState['storage']>(
    state => state.dryRun && state.dryRun.storage
  );

  return (
    <Container>
      <Group>
        <AccessFunctionLabel htmlFor="entrypoint"></AccessFunctionLabel>
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
          rows={5}
          value={parameters}
          onChange={ev =>
            dispatch({ ...new ChangeParametersAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
      <Group>
        <Label htmlFor="storage">Storage</Label>
        <Textarea
          dark={isDark}
          id="storage"
          rows={5}
          value={storage}
          onChange={ev =>
            dispatch({ ...new ChangeStorageAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
    </Container>
  );
};
