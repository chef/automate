import { createSelector, createFeatureSelector } from '@ngrx/store';
import { filter, includes } from 'lodash/fp';

import { CredentialState, credentialAdapter } from './credential.reducer';

export const credentialState = createFeatureSelector<CredentialState>('credentialEntity');

export const {
  selectIds: credentialIds,
  selectEntities: credentialEntities,
  selectAll: allCredentials,
  selectTotal: totalCredentials
} = credentialAdapter.getSelectors(credentialState);

export const instanceCredentials = createSelector(
  allCredentials,
  (credentials) => filter((cred) => includes(cred.type, ['ssh', 'winrm', 'sudo']), credentials)
);
