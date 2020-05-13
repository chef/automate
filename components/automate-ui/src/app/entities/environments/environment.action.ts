import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Environment } from './environment.model';

export enum EnvironmentActionTypes {
  GET_ALL = 'ENVIRONMENTS::GET_ALL',
  GET_ALL_SUCCESS = 'ENVIRONMENTS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'ENVIRONMENTS::GET_ALL::FAILURE'
}

export interface EnvironmentsSuccessPayload {
  environments: Environment[];
}

export class GetEnvironments implements Action {
  readonly type = EnvironmentActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string }) { }
}

export class GetEnvironmentsSuccess implements Action {
  readonly type = EnvironmentActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: EnvironmentsSuccessPayload) { }
}

export class GetEnvironmentsFailure implements Action {
  readonly type = EnvironmentActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type EnvironmentActions =
  | GetEnvironments
  | GetEnvironmentsSuccess
  | GetEnvironmentsFailure;
