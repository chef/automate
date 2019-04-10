import { createSelector, createFeatureSelector } from '@ngrx/store';

import { PolicyEntityState, policyEntityAdapter } from './policy.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const policyState = createFeatureSelector<PolicyEntityState>('policies');

export const {
  selectAll: allPolicies,
  selectEntities: policyEntities
} = policyEntityAdapter.getSelectors(policyState);

export const iamMajorVersion = createSelector(
  policyState,
  (state) => state.iamMajorVersion
);

export const iamMinorVersion = createSelector(
  policyState,
  (state) => state.iamMinorVersion
);

export const getAllStatus = createSelector(
  policyState,
  (state) => state.getAllStatus
);

export const removePolicyMembersStatus = createSelector(
  policyState,
  (state) => state.removePolicyMembersStatus
);

export const addPolicyMembersStatus = createSelector(
  policyState,
  (state) => state.addPolicyMembersStatus
);

export const getStatus = createSelector(
  policyState,
  (state) => state.getStatus
);

export const policyFromRoute = createSelector(
  policyEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);

export const deleteStatus = createSelector(
  policyState,
  (state) => state.deleteStatus
);

export const addPolicyMembersHTTPError = createSelector(
  policyState,
  (state) => state.addPolicyMembersHTTPError
);

export const removePolicyMembersHTTPError = createSelector(
  policyState,
  (state) => state.removePolicyMembersHTTPError
);
