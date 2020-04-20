import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { CookbookVersions } from './cookbook-versions.model';

export enum CookbookVersionsActionTypes {
  GET = 'COOKBOOK_VERSIONS::GET',
  GET_SUCCESS = 'COOKBOOK_VERSIONS::GET::SUCCESS',
  GET_FAILURE = 'COOKBOOK_VERSIONS::GET_ALL::FAILURE'
}

export class GetCookbookVersions implements Action {
  readonly type = CookbookVersionsActionTypes.GET;

  constructor(public payload: { server_id: string, org_id: string, cookbook_name: string }) { }
}

export class GetCookbookVersionsSuccess implements Action {
  readonly type = CookbookVersionsActionTypes.GET_SUCCESS;

  constructor(public payload: CookbookVersions) { }
}

export class GetCookbookVersionsFailure implements Action {
  readonly type = CookbookVersionsActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type CookbookVersionsActions =
  | GetCookbookVersions
  | GetCookbookVersionsSuccess
  | GetCookbookVersionsFailure;
