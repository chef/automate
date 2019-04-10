import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { Credential } from './credential.model';
import { CredentialActionTypes, CredentialActions } from './credential.actions';
import { EntityStatus } from '../entities';

export interface CredentialState extends EntityState<Credential> {
  status: EntityStatus;
}

export const credentialAdapter: EntityAdapter<Credential> = createEntityAdapter<Credential>();

export const InitialState: CredentialState = credentialAdapter.getInitialState({
  status: EntityStatus.notLoaded
});

export function credentialReducer(state: CredentialState = InitialState,
                                        action: CredentialActions): CredentialState {
  switch (action.type) {

    case CredentialActionTypes.SEARCH:
      return set('status', EntityStatus.loading, state);

    case CredentialActionTypes.SEARCH_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
                 credentialAdapter.addAll(action.payload, state));

    case CredentialActionTypes.SEARCH_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
