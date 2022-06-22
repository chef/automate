import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

export enum RoleEnvironmentActionTypes {
  GET_ALL          = 'ROLEENVIRONMENTS::GET_ALL',
  GET_ALL_SUCCESS  = 'ROLEENVIRONMENTS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE  = 'ROLEENVIRONMENTS::GET_ALL::FAILURE'
}

export interface RoleEnvironmentsSuccessPayload {
  environments: string[];
}

export class GetRoleEnvironments implements Action {
  readonly type = RoleEnvironmentActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetRoleEnvironmentsSuccess implements Action {
  readonly type = RoleEnvironmentActionTypes.GET_ALL_SUCCESS;
  constructor(public payload) { }
}

export class GetRoleEnvironmentsFailure implements Action {
  readonly type = RoleEnvironmentActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type RoleEnvironmentActions =
  | GetRoleEnvironments
  | GetRoleEnvironmentsSuccess
  | GetRoleEnvironmentsFailure;
