import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { CookbookDetails } from './cookbookdetails.model';

export enum CookbookDetailsActionTypes {
  GET = 'COOKBOOKDETAILS::GET',
  GET_SUCCESS = 'COOKBOOKDETAILS::GET::SUCCESS',
  GET_FAILURE = 'COOKBOOKDETAILS::GET::FAILURE'
}

export interface CookbookDetailsSuccessPayload {
  cookbookdetails: CookbookDetails;
}

export class GetCookbookDetailsForVersion implements Action {
  readonly type = CookbookDetailsActionTypes.GET;

  constructor(public payload: { server_id: string, org_id: string, cookbook_name: string, cookbook_version: string }) { }
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
  | GetCookbookDetailsForVersion
  | GetCookbookDetailsSuccess
  | GetCookbookDetailsFailure;
