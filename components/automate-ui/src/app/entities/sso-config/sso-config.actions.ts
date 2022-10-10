import { Action } from '@ngrx/store'
import { HttpErrorResponse } from '@angular/common/http'
import { SsoConfig } from './sso-config.model'

export enum SsoConfigActionTypes {
  GET = 'SSO_CONFIG::GET_ALL',
  GET_SUCCESS = 'SSO_CONFIG::GET::SUCCESS',
  GET_FAILURE = 'SSO_CONFIG::GET::FAILURE'
}

export class GetSsoConfig implements Action {
  readonly type = SsoConfigActionTypes.GET;
}

export class GetSsoConfigSuccess implements Action {
  readonly type = SsoConfigActionTypes.GET_SUCCESS;
  constructor(public payload: SsoConfig) { }
}

export class GetSsoConfigFailure implements Action {
  readonly type = SsoConfigActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { };
}

export type SsoConfigActions =
  | GetSsoConfig
  | GetSsoConfigSuccess
  | GetSsoConfigFailure
