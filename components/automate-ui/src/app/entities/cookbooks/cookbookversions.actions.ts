import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { CookbookVersions } from './cookbookversions.model';

export enum CookbookVersionsActionTypes {
  GET = 'COOKBOOKVERSIONS::GET',
  GET_SUCCESS = 'COOKBOOKVERSIONS::GET::SUCCESS',
  GET_FAILURE = 'COOKBOOKVERSIONS::GET_ALL::FAILURE'
}

export class GetCookbookVersions implements Action {
  readonly type = CookbookVersionsActionTypes.GET;

  constructor(public payload: { server_id: string, org_id: string, cookbook_name: string }) { }
}

// export interface CookbookVersionsSuccessPayload {
//   cookbookversions: CookbookVersions;
// }

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
