import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { SsoConfigActionTypes, SsoConfigActions } from './sso-config.actions';
import { SsoConfig } from './sso-config.model';

export interface SsoConfigEntityState extends EntityState<SsoConfig> {
  getStatus: EntityStatus;
  ssoConfig: SsoConfig;
}

const GET_STATUS = 'getStatus';

export const ssoConfigEntityAdapter:
  EntityAdapter<SsoConfig> = createEntityAdapter<SsoConfig>();

export const SsoConfigEntityInitialState: SsoConfigEntityState =
  ssoConfigEntityAdapter.getInitialState(<SsoConfigEntityState>{
    getStatus: EntityStatus.notLoaded,
    ssoConfig: null
});

export function ssoConfigEntityReducer(
  state: SsoConfigEntityState = SsoConfigEntityInitialState,
  action: SsoConfigActions): SsoConfigEntityState {

  switch (action.type) {
    case SsoConfigActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, ssoConfigEntityAdapter.removeAll(state));

    case SsoConfigActionTypes.GET_SUCCESS:
      return pipe(
        set('getStatus', EntityStatus.loadingSuccess),
        set('ssoConfig', action.payload)
      )(state) as SsoConfigEntityState;

    case SsoConfigActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
