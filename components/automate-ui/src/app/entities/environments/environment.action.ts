import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Environment } from './environment.model';

export enum EnvironmentActionTypes {
  GET = 'ENVIRONMENTS::GET',
  GET_SUCCESS = 'ENVIRONMENTS::GET::SUCCESS',
  GET_FAILURE = 'ENVIRONMENTS::GET::FAILURE',
  GETALL = 'ENVIRONMENTS::GETALL',
  GETALL_SUCCESS = 'ENVIRONMENTS::GETALL::SUCCESS',
  GETALL_FAILURE = 'ENVIRONMENTS::GETALL::FAILURE'
}

export interface EnvironmentsSuccessPayload {
  environments: Environment[];
}

export class GetEnvironment implements Action {
  readonly type = EnvironmentActionTypes.GET;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetEnvironmentSuccess implements Action {
  readonly type = EnvironmentActionTypes.GET_SUCCESS;
  constructor(public payload: Environment) { }
}

export class GetEnvironmentFailure implements Action {
  readonly type = EnvironmentActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export interface EnvironmentGetAllPayload {
  environmentName: string;
  org_id: string;
  page: number;
  per_page: number;
  server_id: string;
}

export class EnvironmentGetAll implements Action {
  readonly type = EnvironmentActionTypes.GETALL;
  constructor(public payload: EnvironmentGetAllPayload) { }
}

export interface EnvironmentGetAllSuccessPayload {
  environments: Environment[];
  total: number;
}

export class EnvironmentGetAllSuccess implements Action {
  readonly type = EnvironmentActionTypes.GETALL_SUCCESS;
  constructor(public payload: EnvironmentGetAllSuccessPayload) { }
}

export class EnvironmentGetAllFailure implements Action {
  readonly type = EnvironmentActionTypes.GETALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type EnvironmentActions =
  | GetEnvironment
  | GetEnvironmentSuccess
  | GetEnvironmentFailure
  | EnvironmentGetAll
  | EnvironmentGetAllSuccess
  | EnvironmentGetAllFailure;

