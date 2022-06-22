import { createSelector, createFeatureSelector } from '@ngrx/store';
import { PolicyGroupEntityState, policyGroupEntityAdapter } from './policy-group.reducer';

export const policyFileState = createFeatureSelector<PolicyGroupEntityState>('policyGroups');

export const {
  selectAll: allPolicyFiles,
  selectEntities: policyFileEntities
} = policyGroupEntityAdapter.getSelectors(policyFileState);

export const getGroupsStatus = createSelector(
  policyFileState,
  (state) => state.getGroupsStatus
);

export const policyFile = createSelector(
  policyFileState,
  (state) => state.policyFile
);

export const infraPolicyGroupFromRoute = createSelector(
  policyFileState,
  (state) => state.policyFile
);
