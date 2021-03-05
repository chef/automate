import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Environment } from './environment.model';

export enum EnvironmentActionTypes {
  GET_ALL = 'ENVIRONMENTS::GET_ALL',
  GET_ALL_SUCCESS = 'ENVIRONMENTS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'ENVIRONMENTS::GET_ALL::FAILURE',
  GET = 'ENVIRONMENTS::GET',
  GET_SUCCESS = 'ENVIRONMENTS::GET::SUCCESS',
  GET_FAILURE = 'ENVIRONMENTS::GET::FAILURE',
  DELETE          = 'ENVIRONMENTS::DELETE',
  DELETE_SUCCESS  = 'ENVIRONMENTS::DELETE::SUCCESS',
  DELETE_FAILURE  = 'ENVIRONMENTS::DELETE::FAILURE'
}

export interface GetEnvironmentsPayload {
  environmentName: string;
  org_id: string;
  page: number;
  per_page: number;
  server_id: string;
}

export class GetEnvironments implements Action {
  readonly type = EnvironmentActionTypes.GET_ALL;
  constructor(public payload: GetEnvironmentsPayload) { }
}

export interface GetEnvironmentsSuccessPayload {
  environments: Environment[];
  total: number;
}

export class GetEnvironmentsSuccess implements Action {
  readonly type = EnvironmentActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: GetEnvironmentsSuccessPayload) { }
}

export class GetEnvironmentsFailure implements Action {
  readonly type = EnvironmentActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
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

export class DeleteEnvironment implements Action {
  readonly type = EnvironmentActionTypes.DELETE;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class DeleteEnvironmentSuccess implements Action {
  readonly type = EnvironmentActionTypes.DELETE_SUCCESS;
  constructor(public payload: { name: string }) { }
}

export class DeleteEnvironmentFailure implements Action {
  readonly type = EnvironmentActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type EnvironmentActions =
  | GetEnvironments
  | GetEnvironmentsSuccess
  | GetEnvironmentsFailure
  | GetEnvironment
  | GetEnvironmentSuccess
  | GetEnvironmentFailure
  | DeleteEnvironment
  | DeleteEnvironmentSuccess
  | DeleteEnvironmentFailure;
