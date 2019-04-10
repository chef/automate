import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import {
  LicenseStatus,
  ApplyLicensePayload,
  ApplyLicenseResponse,
  RequestLicensePayload,
  RequestLicenseResponse
} from './license.model';

export enum LicenseStatusActionTypes {
  GET = 'LICENSE_STATUS::GET',
  GET_SUCCESS = 'LICENSE_STATUS::GET::SUCCESS',
  GET_SUCCESS_EXPIRING_SOON = 'LICENSE_STATUS::GET::SUCCESS_EXPIRING_SOON',
  GET_FAILURE = 'LICENSE_STATUS::GET::FAILURE',

  APPLY = 'LICENSE::APPLY',
  APPLY_SUCCESS = 'LICENSE::APPLY::SUCCESS',
  APPLY_FAILURE = 'LICENSE::APPLY::FAILURE',

  REQUEST = 'LICENSE::REQUEST',
  REQUEST_SUCCESS = 'LICENSE::REQUEST::SUCCESS',
  REQUEST_FAILURE = 'LICENSE::REQUEST::FAILURE',

  TRIGGER_WELCOME = 'LICENSE::TRIGGER_WELCOME'
}

export class GetLicenseStatus implements Action {
  readonly type = LicenseStatusActionTypes.GET;
}

export class GetLicenseStatusSuccess implements Action {
  readonly type = LicenseStatusActionTypes.GET_SUCCESS;

  constructor(public payload: LicenseStatus) {}
}

export class GetLicenseStatusSuccessExpiringSoon implements Action {
  readonly type = LicenseStatusActionTypes.GET_SUCCESS_EXPIRING_SOON;

  constructor(public payload: { license: LicenseStatus, expiry_message: string}) {}
}

export class GetLicenseStatusFailure implements Action {
  readonly type = LicenseStatusActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse) {}
}

export class ApplyLicense implements Action {
  readonly type = LicenseStatusActionTypes.APPLY;

  constructor(public payload: ApplyLicensePayload) {}
}

export class ApplyLicenseSuccess implements Action {
  readonly type = LicenseStatusActionTypes.APPLY_SUCCESS;

  constructor(public payload: ApplyLicenseResponse) {}
}

export class ApplyLicenseFailure implements Action {
  readonly type = LicenseStatusActionTypes.APPLY_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class RequestLicense implements Action {
  readonly type = LicenseStatusActionTypes.REQUEST;

  constructor(public payload: RequestLicensePayload) { }
}

export class RequestLicenseSuccess implements Action {
  readonly type = LicenseStatusActionTypes.REQUEST_SUCCESS;

  constructor(public payload: RequestLicenseResponse) {}
}

export class RequestLicenseFailure implements Action {
  readonly type = LicenseStatusActionTypes.REQUEST_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class TriggerWelcome implements Action {
  readonly type = LicenseStatusActionTypes.TRIGGER_WELCOME;
}

export type LicenseStatusAction =
  | GetLicenseStatus
  | GetLicenseStatusSuccess
  | GetLicenseStatusSuccessExpiringSoon
  | GetLicenseStatusFailure
  | ApplyLicense
  | ApplyLicenseSuccess
  | ApplyLicenseFailure
  | RequestLicense
  | RequestLicenseSuccess
  | RequestLicenseFailure
  | TriggerWelcome;
