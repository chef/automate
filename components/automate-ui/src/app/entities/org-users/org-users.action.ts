import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { OrgUser } from './org-users.model';

export enum OrgUsersActionTypes {
  GET_ALL           = 'USERS::GET_ALL',
  GET_ALL_SUCCESS   = 'USERS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE   = 'USERS::GET_ALL::FAILURE',
  RESETKEY          = 'USERS::RESETKEY',
  RESETKEY_SUCCESS  = 'USERS::RESETKEY::SUCCESS',
  RESETKEY_FAILURE  = 'USERS::RESETKEY::FAILURE'
}

export interface UsersSuccessPayload {
  users: OrgUser[];
}

export interface UsersPayload {
  org_id: string;
  server_id: string;
}

export class GetUsers implements Action {
  readonly type = OrgUsersActionTypes.GET_ALL;
  constructor(public payload: UsersPayload) { }
}

export class GetUsersSuccess implements Action {
  readonly type = OrgUsersActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: UsersSuccessPayload) { }
}

export class GetUsersFailure implements Action {
  readonly type = OrgUsersActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class ResetUserKey implements Action {
  readonly type = OrgUsersActionTypes.RESETKEY;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export interface ResetKeySuccessPayload {
  name: string;
  key: {
    name: string,
    public_key: string,
    expiration_date: string,
    private_key: string
  };
}

export class ResetUserKeySuccess implements Action {
  readonly type = OrgUsersActionTypes.RESETKEY_SUCCESS;
  constructor(public payload: ResetKeySuccessPayload ) { }
}

export class ResetUserKeyFailure implements Action {
  readonly type = OrgUsersActionTypes.RESETKEY_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type OrgUsersActions =
  | GetUsers
  | GetUsersSuccess
  | GetUsersFailure
  | ResetUserKey
  | ResetUserKeySuccess
  | ResetUserKeyFailure;
