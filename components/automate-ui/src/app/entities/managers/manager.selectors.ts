import { createSelector, createFeatureSelector } from '@ngrx/store';
import { find, filter } from 'lodash/fp';

import { routeParams } from '../../route.selectors';
import { ManagerEntityState, managerEntityAdapter } from './manager.reducer';

export const managerState = createFeatureSelector<ManagerEntityState>('managers');

export const {
  selectIds: managerIds,
  selectEntities: managerEntities,
  selectAll: allManagers,
  selectTotal: totalManagers
} = managerEntityAdapter.getSelectors(managerState);

export const managerStatus = createSelector(
  managerState,
  (state) => state.status
);

export const fieldsByManager = createSelector(
  managerState,
  (state) => state.fieldsByManager
);

export const nodesByManager = createSelector(
  managerState,
  (state) => state.nodesByManager
);

export const managerFromRoute = createSelector(
  managerEntities,
  routeParams,
  (state, {id}) => state[id]
);

export const managerNodesFromRoute = createSelector(
  nodesByManager,
  routeParams,
  (state, {id}) => state[id]
);

export const automateManager = createSelector(
  allManagers,
  find((manager) => manager.type === 'automate')
);

export const cloudManagers = createSelector(
  allManagers,
  (state) => filter((manager) => manager.type !== 'automate', state)
);
