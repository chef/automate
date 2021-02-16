import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Environment } from './environment.model';

export enum EnvironmentActionTypes {
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
  total: number;
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
  | GetEnvironment
  | GetEnvironmentSuccess
  | GetEnvironmentFailure
  | EnvironmentSearch
  | EnvironmentSearchSuccess
  | EnvironmentSearchFailure;
  