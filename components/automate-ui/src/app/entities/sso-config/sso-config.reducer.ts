import { HttpErrorResponse } from '@angular/common/http';
import { createEntityAdapter, EntityAdapter, EntityState } from '@ngrx/entity';
import { pipe, set, unset } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { SsoConfigActions, SsoConfigActionTypes } from './sso-config.actions';
import { SsoConfig } from './sso-config.model';

export interface SsoConfigEntityState extends EntityState<SsoConfig> {
  ssoConfig: SsoConfig;
  getStatus: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
  deleteStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';
const DELETE_STATUS = 'deleteStatus';

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

    case SsoConfigActionTypes.CREATE: {
      return set(
        SAVE_STATUS,
        EntityStatus.loading,
        state);
    }

    case SsoConfigActionTypes.CREATE_SUCCESS: {
      return pipe(
        unset(SAVE_ERROR),
        set(SAVE_STATUS, EntityStatus.loadingSuccess)
      )(state) as SsoConfigEntityState;
    }

    case SsoConfigActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as SsoConfigEntityState;
    }

    case SsoConfigActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case SsoConfigActionTypes.DELETE_SUCCESS:
      return pipe(
        set(DELETE_STATUS, EntityStatus.loadingSuccess),
        set('ssoConfig', null),
      )(state) as SsoConfigEntityState;

    case SsoConfigActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
