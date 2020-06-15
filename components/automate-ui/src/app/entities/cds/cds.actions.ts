import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { ContentItem } from './cds.model';

export enum CdsActionTypes {
  GET_CONTENT_ITEMS           = 'CDS::GET::CONTENT_ITEMS',
  GET_CONTENT_ITEMS_SUCCESS   = 'CDS::GET::CONTENT_ITEMS::SUCCESS',
  GET_CONTENT_ITEMS_FAILURE   = 'CDS::GET::CONTENT_ITEMS::FAILURE'
}

export class GetContentItems implements Action {
  readonly type = CdsActionTypes.GET_CONTENT_ITEMS;
}

export class GetContentItemsSuccess implements Action {
  readonly type = CdsActionTypes.GET_CONTENT_ITEMS_SUCCESS;
  constructor(public payload: ContentItem[]) { }
}

export class GetContentItemsFailure implements Action {
  readonly type = CdsActionTypes.GET_CONTENT_ITEMS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type CdsActions =
  | GetContentItems
  | GetContentItemsSuccess
  | GetContentItemsFailure;
