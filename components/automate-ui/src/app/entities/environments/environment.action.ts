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
  SEARCH = 'ENVIRONMENTS::SEARCH',
  SEARCH_SUCCESS = 'ENVIRONMENTS::SEARCH::SUCCESS',
  SEARCH_FAILURE = 'ENVIRONMENTS::SEARCH::FAILURE'
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

export interface EnvironmentSearchPayload {
  environmentName: string;
  org_id: string;
  page: number;
  per_page: number;
  server_id: string;
}

export class EnvironmentSearch implements Action {
  readonly type = EnvironmentActionTypes.SEARCH;
  constructor(public payload: EnvironmentSearchPayload) { }
}

export interface EnvironmentSearchSuccessPayload {
  environments: Environment[];
}

export class EnvironmentSearchSuccess implements Action {
  readonly type = EnvironmentActionTypes.SEARCH_SUCCESS;
  constructor(public payload: EnvironmentSearchSuccessPayload) { }
}

export class EnvironmentSearchFailure implements Action {
  readonly type = EnvironmentActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type EnvironmentActions =
  | GetEnvironments
  | GetEnvironmentsSuccess
  | GetEnvironmentsFailure
  | GetEnvironment
  | GetEnvironmentSuccess
  | GetEnvironmentFailure
  | EnvironmentSearch
  | EnvironmentSearchSuccess
  | EnvironmentSearchFailure;
