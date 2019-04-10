import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { LicenseStatus } from './license.model';
import { LicenseStatusActionTypes, LicenseStatusAction } from './license.actions';

export interface LicenseStatusEntityState {
  fetch: FetchStatus;
  apply: ApplyStatus;
  request: RequestStatus;
  triggerWelcome: TriggerWelcomeStatus;
}

export interface FetchStatus {
  license: LicenseStatus;
  status: EntityStatus;
  expiryMessage: string;
  errorResp: HttpErrorResponse;
}

export interface ApplyStatus {
  status: EntityStatus;
  errorResp: HttpErrorResponse;
}

export interface RequestStatus {
  status: EntityStatus;
  errorResp: HttpErrorResponse;
}

export interface TriggerWelcomeStatus {
  status: EntityStatus;
}

export const LicenseStatusEntityInitialState: LicenseStatusEntityState = {
  fetch: {
    license: null,
    status: EntityStatus.notLoaded,
    expiryMessage: '',
    errorResp: null
  },
  apply: {
    status: EntityStatus.notLoaded,
    errorResp: null
  },
  request: {
    status: EntityStatus.notLoaded,
    errorResp: null
  },
  triggerWelcome: {
    status: EntityStatus.notLoaded
  }
};

export function licenseStatusEntityReducer(
  state: LicenseStatusEntityState = LicenseStatusEntityInitialState,
  action: LicenseStatusAction): LicenseStatusEntityState  {

  switch (action.type) {

    case LicenseStatusActionTypes.GET: {
      return set('fetch.status', EntityStatus.loading, state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.GET_SUCCESS_EXPIRING_SOON: {
      return pipe(
        set('fetch.status', EntityStatus.loadingSuccess),
        set('fetch.expiryMessage', action.payload.expiry_message),
        set('fetch.errorResp', null),
        set('fetch.license', action.payload.license)
      )(state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.GET_SUCCESS: {
      return pipe(
        set('fetch.status', EntityStatus.loadingSuccess),
        set('fetch.expiryMessage', ''),
        set('fetch.errorResp', null),
        set('fetch.license', action.payload)
      )(state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.GET_FAILURE: {
      return pipe(
        set('fetch.status', EntityStatus.loadingFailure),
        set('fetch.errorResp', action.payload)
      )(state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.APPLY: {
      return set('apply.status', EntityStatus.loading, state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.APPLY_SUCCESS: {
      return pipe(
        set('apply.status', EntityStatus.loadingSuccess),
        set('fetch.status', EntityStatus.loadingSuccess),
        set('fetch.expiryMessage', ''),
        set('fetch.errorResp', null),
        set('fetch.license', action.payload.status)
      )(state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.APPLY_FAILURE: {
      return pipe(
        set('apply.status', EntityStatus.loadingFailure),
        set('apply.errorResp', action.payload)
      )(state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.REQUEST: {
      return set('request.status', EntityStatus.loading, state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.REQUEST_SUCCESS: {
      return pipe(
        set('request.status', EntityStatus.loadingSuccess),
        set('fetch.status', EntityStatus.loadingSuccess),
        set('fetch.expiryMessage', ''),
        set('fetch.errorResp', null),
        set('fetch.license', action.payload.status)
      )(state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.REQUEST_FAILURE: {
      return pipe(
        set('request.status', EntityStatus.loadingFailure),
        set('request.errorResp', action.payload)
      )(state) as LicenseStatusEntityState;
    }

    case LicenseStatusActionTypes.TRIGGER_WELCOME: {
      return set(
        'triggerWelcome.status', EntityStatus.loadingSuccess, state
      ) as LicenseStatusEntityState;
    }
  }

  return state;
}

export type LicenseStatusType =
  | FetchStatus
  | ApplyStatus
  | RequestStatus;
