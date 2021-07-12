import { createSelector, createFeatureSelector } from '@ngrx/store';
import { find } from 'lodash/fp';
import { PolicyFileEntityState, policyFileEntityAdapter } from './policy-file.reducer';
import { routeParams } from 'app/route.selectors';

export const policyFileState = createFeatureSelector<PolicyFileEntityState>('policyFiles');
export const {
  selectAll: allPolicyFiles,
  selectEntities: policyFileEntities
} = policyFileEntityAdapter.getSelectors(policyFileState);

export const getAllStatus = createSelector(
  policyFileState,
  (state) => state.getAllStatus
);

export const infraPolicyFileFromRoute = createSelector(
  policyFileEntities,
  routeParams,
  (state, { revision_id }) => find({ revision_id }, state)
);

export const deleteStatus = createSelector(
  policyFileState,
  (state) => state.deleteStatus
);
