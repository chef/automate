import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { Role } from './role.model';

export enum RoleActionTypes {
  DELETE          = 'ROLE::DELETE',
  DELETE_SUCCESS  = 'ROLE::DELETE::SUCCESS',
  DELETE_FAILURE  = 'ROLE::DELETE::FAILURE',
  GET_ALL         = 'ROLE::GET_ALL',
  GET_ALL_SUCCESS = 'ROLE::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'ROLE::GET_ALL::FAILURE',
  GET             = 'ROLE::GET',
  GET_SUCCESS     = 'ROLE::GET::SUCCESS',
  GET_FAILURE     = 'ROLE::GET::FAILURE'
}

export class GetRoles implements Action {
  readonly type = RoleActionTypes.GET_ALL;
}

export interface GetRolesSuccessPayload {
  roles: Role[];
}

export class GetRolesSuccess implements Action {
  readonly type = RoleActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: GetRolesSuccessPayload) { }
}

export class GetRolesFailure implements Action {
  readonly type = RoleActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetRole implements Action {
  readonly type = RoleActionTypes.GET;

  constructor(public payload: { id: string }) { }
}

export class GetRoleSuccess implements Action {
  readonly type = RoleActionTypes.GET_SUCCESS;

  constructor(public payload: Role) { }
}

export class GetRoleFailure implements Action {
  readonly type = RoleActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse, public id: string) { }
}

export class DeleteRole implements Action {
  readonly type = RoleActionTypes.DELETE;
  constructor(public payload: { id: string }) { }
}

export class DeleteRoleSuccess implements Action {
  readonly type = RoleActionTypes.DELETE_SUCCESS;
  constructor(public payload: { id: string }) { }
}

export class DeleteRoleFailure implements Action {
  readonly type = RoleActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type RoleActions =
  | DeleteRole
  | DeleteRoleSuccess
  | DeleteRoleFailure
  | GetRoles
  | GetRolesSuccess
  | GetRolesFailure
  | GetRole
  | GetRoleSuccess
  | GetRoleFailure;
