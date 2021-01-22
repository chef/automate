import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { InfraRole } from './infra-role.model';

export enum RoleActionTypes {
  DELETE          = 'ROLES::DELETE',
  DELETE_SUCCESS  = 'ROLES::DELETE::SUCCESS',
  DELETE_FAILURE  = 'ROLES::DELETE::FAILURE',
  GET_ALL = 'ROLES::GET_ALL',
  GET_ALL_SUCCESS = 'ROLES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'ROLES::GET_ALL::FAILURE',
  GET = 'ROLES::GET',
  GET_SUCCESS = 'ROLES::GET::SUCCESS',
  GET_FAILURE = 'ROLES::GET::FAILURE'
}

export interface RolesSuccessPayload {
  roles: InfraRole[];
  total: number;
}

export interface RolesPayload {
  roleName: string;
  server_id: string;
  org_id: string;
  page: number;
  per_page: number;
}

export class DeleteRole implements Action {
  readonly type = RoleActionTypes.DELETE;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class DeleteRoleSuccess implements Action {
  readonly type = RoleActionTypes.DELETE_SUCCESS;
  constructor(public payload: { name: string }) { }
}

export class DeleteRoleFailure implements Action {
  readonly type = RoleActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetRoles implements Action {
  readonly type = RoleActionTypes.GET_ALL;
  constructor(public payload: RolesPayload) { }
}

export class GetRolesSuccess implements Action {
  readonly type = RoleActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: RolesSuccessPayload) { }
}

export class GetRolesFailure implements Action {
  readonly type = RoleActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetRole implements Action {
  readonly type = RoleActionTypes.GET;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetRoleSuccess implements Action {
  readonly type = RoleActionTypes.GET_SUCCESS;
  constructor(public payload: InfraRole) { }
}

export class GetRoleFailure implements Action {
  readonly type = RoleActionTypes.GET_FAILURE;
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
