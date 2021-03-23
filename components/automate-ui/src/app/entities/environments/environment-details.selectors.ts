import { createSelector, createFeatureSelector } from '@ngrx/store';
import { find } from 'lodash/fp';

import { routeParams } from 'app/route.selectors';
import { EnvironmentDetailsEntityState, environmentDetailsEntityAdapter } from './environment-details.reducer';

export const environmentState =
createFeatureSelector<EnvironmentDetailsEntityState>('environmentDetails');

export const {
  selectAll: allEnvironments,
  selectEntities: environmentEntities
} = environmentDetailsEntityAdapter.getSelectors(environmentState);

export const environmentStatus = createSelector(
  environmentState,
  (state) => state.environmentStatus
);

export const getStatus = createSelector(
  environmentState,
  (state) => state.getStatus
);

export const updateStatus = createSelector(
  environmentState,
  (state) => state.updateStatus
);

export const environmentFromRoute = createSelector(
  environmentEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
