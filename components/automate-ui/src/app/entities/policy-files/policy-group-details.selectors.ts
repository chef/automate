import { createSelector, createFeatureSelector } from '@ngrx/store';
import { PolicyGroupDetailsEntityState, policyGroupDetailsEntityAdapter } from './policy-group-details.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const policyFileState = createFeatureSelector<PolicyGroupDetailsEntityState>('policyGroupDetails');

export const {
  selectAll: allPolicyFiles,
  selectEntities: policyFileEntities
} = policyGroupDetailsEntityAdapter.getSelectors(policyFileState);

export const policyGroupStatus = createSelector(
  policyFileState,
  (state) => state.policyGroupStatus
);

export const getStatus = createSelector(
  policyFileState,
  (state) => state.getStatus
);

export const policyGoupFromRoute = createSelector(
  policyFileEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
