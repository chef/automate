import { Action } from '@ngrx/store'
import { HttpErrorResponse } from '@angular/common/http'
import { Sso } from './sso.model'

export enum SsoActionTypes {
    GET_ALL         = 'SSO::GET_ALL',
    GET_ALL_SUCCESS = 'SSO::GET_ALL::SUCCESS',
    GET_ALL_FAILURE = 'SSO::GET_ALL::FAILURE'
}

export class GetAllSsoConfig implements Action { 
    readonly type = SsoActionTypes.GET_ALL;
}

export interface GetAllConfigSuccessPayload {
    sso: Sso[];
}

export class GetAllSsoConfigSuccess implements Action {
    readonly type = SsoActionTypes.GET_ALL_SUCCESS;
    constructor(public payload: GetAllConfigSuccessPayload) {}
}

export class GetAllSsoConfigFailure implements Action {
    readonly type = SsoActionTypes.GET_ALL_FAILURE;
    constructor(public payload: HttpErrorResponse) {};
}

export type SsoActions = 
  | GetAllSsoConfig
  | GetAllSsoConfigSuccess
  | GetAllSsoConfigFailure