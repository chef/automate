import { createSelector, createFeatureSelector } from '@ngrx/store';
import { NodeCredentialEntityState, nodeCredentialEntityAdapter } from './node-credential.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';
// import { filter, includes } from 'lodash/fp';

export const nodeCredentialState =
createFeatureSelector<NodeCredentialEntityState>('nodeCredential');

export const {
  selectAll: allCredentials,
  selectEntities: credentialEntities
} = nodeCredentialEntityAdapter.getSelectors(nodeCredentialState);

export const totalNodeCredential = createSelector(
  nodeCredentialState,
  (state) => state.total
);

export const nodeCredentialStatus = createSelector(
  nodeCredentialState,
  (state) => state.nodeCredentialsStatus
);

export const getAllStatus = createSelector(
  nodeCredentialState,
  (state) => state.getAllStatus
);

export const saveStatus = createSelector(
  nodeCredentialState,
  (state) => state.saveStatus
);

export const saveError = createSelector(
  nodeCredentialState,
  (state) => state.saveError
);

export const getStatus = createSelector(
  nodeCredentialState,
  (state) => state.getStatus
);

export const nodeCredentialFromRoute = createSelector(
  credentialEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);

export const updateStatus = createSelector(
  nodeCredentialState,
  (state) => state.updateStatus
);

export const deleteStatus = createSelector(
  credentialEntities,
  (state) => state.deleteStatus
);
