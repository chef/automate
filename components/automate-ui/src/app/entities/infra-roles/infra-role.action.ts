import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { InfraRole } from './infra-role.model';

export enum RoleActionTypes {
  CREATE          = 'ROLES::CREATE',
  CREATE_SUCCESS  = 'ROLES::CREATE::SUCCESS',
  CREATE_FAILURE  = 'ROLES::CREATE::FAILURE',
  DELETE          = 'ROLES::DELETE',
  DELETE_SUCCESS  = 'ROLES::DELETE::SUCCESS',
  DELETE_FAILURE  = 'ROLES::DELETE::FAILURE',
  GET_ALL         = 'ROLES::GET_ALL',
  GET_ALL_SUCCESS = 'ROLES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'ROLES::GET_ALL::FAILURE',
  GET             = 'ROLES::GET',
  GET_SUCCESS     = 'ROLES::GET::SUCCESS',
  GET_FAILURE     = 'ROLES::GET::FAILURE',
  UPDATE          = 'ROLES::UPDATE',
  UPDATE_SUCCESS  = 'ROLES::UPDATE::SUCCESS',
  UPDATE_FAILURE  = 'ROLES::UPDATE::FAILURE'
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

export interface CreateRolePayload {
  org_id: string;
  server_id: string;
  name: string;
  description: string;
  default_attributes: string;
  override_attributes: string;
  run_list: string[];
}

export class CreateRole implements Action {
  readonly type = RoleActionTypes.CREATE;
  constructor(public payload: { role: CreateRolePayload }) { }
}

export class CreateRoleSuccess implements Action {
  readonly type = RoleActionTypes.CREATE_SUCCESS;
  constructor(public payload) { }
}

export class CreateRoleFailure implements Action {
  readonly type = RoleActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
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

export class UpdateRole implements Action {
  readonly type = RoleActionTypes.UPDATE;
  constructor(public payload: InfraRole ) { }
}

export class UpdateRoleSuccess implements Action {
  readonly type = RoleActionTypes.UPDATE_SUCCESS;
  constructor(public payload: InfraRole) { }
}

export class UpdateRoleFailure implements Action {
  readonly type = RoleActionTypes.UPDATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type RoleActions =
  | CreateRole
  | CreateRoleSuccess
  | CreateRoleFailure
  | DeleteRole
  | DeleteRoleSuccess
  | DeleteRoleFailure
  | GetRoles
  | GetRolesSuccess
  | GetRolesFailure
  | GetRole
  | GetRoleSuccess
  | GetRoleFailure
  | UpdateRole
  | UpdateRoleSuccess
  | UpdateRoleFailure;
