import { createSelector, createFeatureSelector } from '@ngrx/store';
import { PolicyFileDetailsEntityState, policyFileDetailsEntityAdapter } from './policy-file-details.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const policyFileState =
  createFeatureSelector<PolicyFileDetailsEntityState>('policyFileDetails');

export const {
  selectAll: allInfraNodes,
  selectEntities: policyFileEntities
} = policyFileDetailsEntityAdapter.getSelectors(policyFileState);

export const policyFileStatus = createSelector(
  policyFileState,
  (state) => state.policyFileStatus
);

export const getStatus = createSelector(
  policyFileState,
  (state) => state.getStatus
);

export const policyFileFromRoute = createSelector(
  policyFileEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
