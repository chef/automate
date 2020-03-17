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
}

export interface RoleSuccessPayload {
  name: InfraRole;
}

export class GetRolesForOrg implements Action {
  readonly type = RoleActionTypes.GET_ALL;

  constructor(public payload: { server_id: string, org_id: string }) { }
}

export interface RolesSuccessPayload {
  roles: InfraRole[];
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
  | GetRolesForOrg
  | GetRolesSuccess
  | GetRolesFailure
  | GetRole
  | GetRoleSuccess
  | GetRoleFailure;
