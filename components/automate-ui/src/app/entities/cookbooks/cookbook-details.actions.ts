import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { CookbookDetails } from './cookbook-details.model';

export enum CookbookDetailsActionTypes {
  GET = 'COOKBOOK_DETAILS::GET',
  GET_SUCCESS = 'COOKBOOK_DETAILS::GET::SUCCESS',
  GET_FAILURE = 'COOKBOOK_DETAILS::GET::FAILURE'
}

export class GetCookbookDetails implements Action {
  readonly type = CookbookDetailsActionTypes.GET;

  constructor(public payload: {
    server_id: string,
    org_id: string,
    cookbook_name: string,
    cookbook_version: string
  }) { }
}

export class GetCookbookDetailsSuccess implements Action {
  readonly type = CookbookDetailsActionTypes.GET_SUCCESS;

  constructor(public payload: CookbookDetails) { }
}

export class GetCookbookDetailsFailure implements Action {
  readonly type = CookbookDetailsActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type CookbookDetailsActions =
  | GetCookbookDetails
  | GetCookbookDetailsSuccess
  | GetCookbookDetailsFailure;
