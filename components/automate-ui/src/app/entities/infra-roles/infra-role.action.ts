import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { InfraRole } from './infra-role.model';

export enum RoleActionTypes {
  GET_ALL = 'ROLES::GET_ALL',
  GET_ALL_SUCCESS = 'ROLES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'ROLES::GET_ALL::FAILURE',
  GET = 'ROLES::GET',
  GET_SUCCESS = 'ROLES::GET::SUCCESS',
  GET_FAILURE = 'ROLES::GET::FAILURE',
  SEARCH = 'ROLES::SEARCH',
  SEARCH_SUCCESS = 'ROLES::SEARCH::SUCCESS',
  SEARCH_FAILURE = 'ROLES::SEARCH::FAILURE'
}

export interface RolesSuccessPayload {
  roles: InfraRole[];
}

export class GetRoles implements Action {
  readonly type = RoleActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string }) { }
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

export interface RoleSearchPayload {
  roleName: string;
  server_id: string;
  org_id: string;
  page: number;
  per_page: number;
}

export class RoleSearch implements Action {
  readonly type = RoleActionTypes.SEARCH;
  constructor(public payload: RoleSearchPayload) { }
}

export interface RoleSearchSuccessPayload {
  roles: InfraRole[];
}

export class RoleSearchSuccess implements Action {
  readonly type = RoleActionTypes.SEARCH_SUCCESS;
  constructor(public payload: RoleSearchSuccessPayload) { }
}

export class RoleSearchFailure implements Action {
  readonly type = RoleActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type RoleActions =
  | GetRoles
  | GetRolesSuccess
  | GetRolesFailure
  | GetRole
  | GetRoleSuccess
  | GetRoleFailure
  | RoleSearch
  | RoleSearchSuccess
  | RoleSearchFailure;
