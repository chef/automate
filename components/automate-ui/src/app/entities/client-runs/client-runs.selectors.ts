import { createSelector, createFeatureSelector } from '@ngrx/store';

import {
  ClientRunsEntityState
} from './client-runs.reducer';

export const clientRunsState =
  createFeatureSelector<ClientRunsEntityState>('clientRunsEntity');

export const clientRunsNodes = createSelector(
  clientRunsState,
  (state) => state.nodes
);

export const clientRunsColumns = createSelector(
  clientRunsState,
  (state) => state.columns
);

export const clientRunsLoading = createSelector(
  clientRunsState,
  (state) => state.status
);
