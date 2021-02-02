import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { DataBags } from './data-bags.model';

export enum DataBagDetailsActionTypes {
  GET_ALL = 'DATA_BAG_DETAILS::GET_ALL',
  GET_ALL_SUCCESS = 'DATA_BAG_DETAILS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'DATA_BAG_DETAILS::GET_ALL::FAILURE',
  SEARCH = 'DATA_BAG_DETAILS::SEARCH',
  SEARCH_SUCCESS = 'DATA_BAG_DETAILS::SEARCH::SUCCESS',
  SEARCH_FAILURE = 'DATA_BAG_DETAILS::SEARCH::FAILURE'
}

export interface DataBagDetailsSuccessPayload {
  data_bags: DataBags[];
}

export interface DataBagItemListsSuccessPayload {
  items: DataBags[];
}

export class GetDataBagDetails implements Action {
  readonly type = DataBagDetailsActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetDataBagDetailsSuccess implements Action {
  readonly type = DataBagDetailsActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: DataBagItemListsSuccessPayload) { }
}

export class GetDataBagDetailsFailure implements Action {
  readonly type = DataBagDetailsActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export interface DataBagSearchPayload {
  databagId: string;
  server_id: string;
  org_id: string;
  name: string;
  page: number;
  per_page: number;
}

export class DataBagSearchDetails implements Action {
  readonly type = DataBagDetailsActionTypes.SEARCH;
  constructor(public payload: DataBagSearchPayload) { }
}

export interface DataBagSearchSuccessPayload {
  items: DataBags[];
}

export class DataBagSearchDetailsSuccess implements Action {
  readonly type = DataBagDetailsActionTypes.SEARCH_SUCCESS;
  constructor(public payload: DataBagSearchSuccessPayload) { }
}

export class DataBagSearchdDetailsFailure implements Action {
  readonly type = DataBagDetailsActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DataBagDetailsActions =
  | GetDataBagDetails
  | GetDataBagDetailsSuccess
  | GetDataBagDetailsFailure
  | GetDataBagDetailsFailure
  | DataBagSearchDetails
  | DataBagSearchDetailsSuccess
  | DataBagSearchdDetailsFailure;
