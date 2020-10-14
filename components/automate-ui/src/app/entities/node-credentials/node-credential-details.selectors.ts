import { createSelector, createFeatureSelector } from '@ngrx/store';
import { NodeCredentialDetailsEntityState, nodeCredentialDetailsEntityAdapter } from './node-credential-details.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const nodeCredentialState =
createFeatureSelector<NodeCredentialDetailsEntityState>('nodeCredentialDetails');

export const {
  selectAll: allCredentials,
  selectEntities: credentialEntities
} = nodeCredentialDetailsEntityAdapter.getSelectors(nodeCredentialState);

export const credentialStatus = createSelector(
  nodeCredentialState,
  (state) => state.nodeCredentialStatus
);

export const getStatus = createSelector(
  nodeCredentialState,
  (state) => state.getStatus
);

export const credentialFromRoute = createSelector(
  credentialEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);
