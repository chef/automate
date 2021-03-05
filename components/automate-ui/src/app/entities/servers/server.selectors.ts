import { createSelector, createFeatureSelector } from '@ngrx/store';

import { ServerEntityState, serverEntityAdapter } from './server.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const serverState = createFeatureSelector<ServerEntityState>('servers');

export const {
  selectAll: allServers,
  selectEntities: serverEntities
} = serverEntityAdapter.getSelectors(serverState);

export const getAllStatus = createSelector(
  serverState,
  (state) => state.getAllStatus
);

export const saveStatus = createSelector(
  serverState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  serverState,
  (state) => state.saveError
);

export const getStatus = createSelector(
  serverState,
  (state) => state.getStatus
);

export const serverFromRoute = createSelector(
  serverEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);

export const updateStatus = createSelector(
  serverState,
  (state) => state.updateStatus
);

export const deleteStatus = createSelector(
  serverState,
  (state) => state.deleteStatus
);
