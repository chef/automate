import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { OrgUser } from './org-users.model';

export enum OrgUsersActionTypes {
  GET_ALL         = 'USERS::GET_ALL',
  GET_ALL_SUCCESS = 'USERS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'USERS::GET_ALL::FAILURE'
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

export type OrgUsersActions =
  | GetUsers
  | GetUsersSuccess
  | GetUsersFailure;
