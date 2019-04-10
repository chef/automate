import { createSelector, createFeatureSelector } from '@ngrx/store';

import { ApiTokenEntityState, apiTokenEntityAdapter } from './api-token.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const apiTokenState = createFeatureSelector<ApiTokenEntityState>('apiTokens');

export const {
  selectAll: allApiTokens,
  selectTotal: totalApiTokens,
  selectEntities: apiTokenEntities
} = apiTokenEntityAdapter.getSelectors(apiTokenState);

export const apiTokenStatus = createSelector(
  apiTokenState,
  (state) => state.status
);

export const saveStatus = createSelector(
  apiTokenState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  apiTokenState,
  (state) => state.saveError
);

export const deleteStatus = createSelector(
  apiTokenState,
  (state) => state.deleteStatus
);

export const getStatus = createSelector(
  apiTokenState,
  (state) => state.getStatus
);

export const updateStatus = createSelector(
  apiTokenState,
  (state) => state.updateStatus
);

export const apiTokenFromRoute = createSelector(
  apiTokenEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);
