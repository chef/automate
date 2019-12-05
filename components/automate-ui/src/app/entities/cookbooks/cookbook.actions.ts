import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { Cookbook } from './cookbook.model';

export enum CookbookActionTypes {
  GET_ALL = 'COOKBOOKS::GET_ALL',
  GET_ALL_SUCCESS = 'COOKBOOKS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'COOKBOOKS::GET_ALL::FAILURE'
}

export interface CookbookSuccessPayload {
  cookbook: Cookbook;
}

export class GetCookbooksForOrg implements Action {
  readonly type = CookbookActionTypes.GET_ALL;

  constructor(public payload: { server_id: string, org_id: string }) { }
}

export interface CookbooksSuccessPayload {
  cookbooks: Cookbook[];
}

export class GetCookbooksSuccess implements Action {
  readonly type = CookbookActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: CookbooksSuccessPayload) { }
}

export class GetCookbooksFailure implements Action {
  readonly type = CookbookActionTypes.GET_ALL_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type CookbookActions =
  | GetCookbooksForOrg
  | GetCookbooksSuccess
  | GetCookbooksFailure;
