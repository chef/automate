import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { DataBagItems } from './data-bags.model';

export enum DataBagItemsActionTypes {
  GET_ALL = 'DATA_BAG_ITEMS::GET_ALL',
  GET_ALL_SUCCESS = 'DATA_BAG_ITEMS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'DATA_BAG_ITEMS::GET_ALL::FAILURE'
}

export interface DataBagItemsSuccessPayload {
  items: DataBagItems[];
  total: number;
}

export interface DataBagItemPayload {
  databagName: string;
  server_id: string;
  org_id: string;
  name: string;
  page: number;
  per_page: number;
}

export class GetDataBagItems implements Action {
  readonly type = DataBagItemsActionTypes.GET_ALL;
  constructor(public payload: DataBagItemPayload) { }
}

export class GetDataBagItemsSuccess implements Action {
  readonly type = DataBagItemsActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: DataBagItemsSuccessPayload) { }
}

export class GetDataBagItemsFailure implements Action {
  readonly type = DataBagItemsActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DataBagItemsActions =
  | GetDataBagItems
  | GetDataBagItemsSuccess
  | GetDataBagItemsFailure;
