import { Action } from '@ngrx/store'
import { HttpErrorResponse } from '@angular/common/http'
import { SsoConfig } from './sso-config.model'

export enum SsoConfigActionTypes {
  GET            = 'SSO_CONFIG::GET_ALL',
  GET_SUCCESS    = 'SSO_CONFIG::GET::SUCCESS',
  GET_FAILURE    = 'SSO_CONFIG::GET::FAILURE',
  CREATE         = 'SSO_CONFIG::CREATE',
  CREATE_SUCCESS = 'SSO_CONFIG::CREATE::SUCCESS',
  CREATE_FAILURE = 'SSO_CONFIG::CREATE::FAILURE',
  DELETE         = 'SSO_CONFIG::DELETE',
  DELETE_SUCCESS = 'SSO_CONFIG::DELETE::SUCCESS',
  DELETE_FAILURE = 'SSO_CONFIG::DELETE::FAILURE'
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

export class CreateSsoConfig implements Action {
  readonly type = SsoConfigActionTypes.CREATE;
  constructor(public payload: SsoConfig) { }
}

export class CreateSsoConfigSuccess implements Action {
  readonly type = SsoConfigActionTypes.CREATE_SUCCESS;
}

export class CreateSsoConfigFailure implements Action {
  readonly type = SsoConfigActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteSsoConfig implements Action {
  readonly type = SsoConfigActionTypes.DELETE;
}

export class DeleteSsoConfigSuccess implements Action {
  readonly type = SsoConfigActionTypes.DELETE_SUCCESS;
}

export class DeleteSsoConfigFailure implements Action {
  readonly type = SsoConfigActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type SsoConfigActions =
  | GetSsoConfig
  | GetSsoConfigSuccess
  | GetSsoConfigFailure
  | CreateSsoConfig
  | CreateSsoConfigSuccess
  | CreateSsoConfigFailure
  | DeleteSsoConfig
  | DeleteSsoConfigSuccess
  | DeleteSsoConfigFailure;
