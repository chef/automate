import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { DataBagItems, DataBagsItemDetails, DataBagItem  } from './data-bags.model';

export enum DataBagItemsActionTypes {
  GET_ALL = 'DATA_BAG_ITEMS::GET_ALL',
  GET_ALL_SUCCESS = 'DATA_BAG_ITEMS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'DATA_BAG_ITEMS::GET_ALL::FAILURE',
  DELETE          = 'DATA_BAG_ITEMS::DELETE',
  DELETE_SUCCESS  = 'DATA_BAG_ITEMS::DELETE::SUCCESS',
  DELETE_FAILURE  = 'DATA_BAG_ITEMS::DELETE::FAILURE',
  UPDATE = 'DATA_BAG_ITEMS::UPDATE',
  UPDATE_SUCCESS = 'DATA_BAG_ITEMS::UPDATE::SUCCESS',
  UPDATE_FAILURE = 'DATA_BAG_ITEMS::UPDATE::FAILURE',
  CREATE          = 'DATA_BAG_ITEMS::CREATE',
  CREATE_SUCCESS  = 'DATA_BAG_ITEMS::CREATE::SUCCESS',
  CREATE_FAILURE  = 'DATA_BAG_ITEMS::CREATE::FAILURE'
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

export interface CreateDataBagItemSuccessPayload {
  name: string;
  id: string;
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

export class CreateDataBagItem implements Action {
  readonly type = DataBagItemsActionTypes.CREATE;
  constructor(public payload: { dataBagItem: DataBagItem }) { }
}

export class CreateDataBagItemSuccess implements Action {
  readonly type = DataBagItemsActionTypes.CREATE_SUCCESS;
  constructor(public payload: CreateDataBagItemSuccessPayload) { }
}

export class CreateDataBagItemFailure implements Action {
  readonly type = DataBagItemsActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}
export class DeleteDataBagItem implements Action {
  readonly type = DataBagItemsActionTypes.DELETE;
  constructor(public payload: {
    server_id: string,
    org_id: string,
    databag_name: string,
    name: string
  }) { }
}

export class DeleteDataBagItemSuccess implements Action {
  readonly type = DataBagItemsActionTypes.DELETE_SUCCESS;
  constructor(public payload: { name: string }) { }
}

export class DeleteDataBagItemFailure implements Action {
  readonly type = DataBagItemsActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateDataBagItem implements Action {
  readonly type = DataBagItemsActionTypes.UPDATE;
  constructor(public payload: { dataBagItem: DataBagsItemDetails }) { }
}

export class UpdateDataBagItemSuccess implements Action {
  readonly type = DataBagItemsActionTypes.UPDATE_SUCCESS;
  constructor(public payload: DataBagsItemDetails) { }
}

export class UpdateDataBagItemFailure implements Action {
  readonly type = DataBagItemsActionTypes.UPDATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DataBagItemsActions =
  | GetDataBagItems
  | GetDataBagItemsSuccess
  | GetDataBagItemsFailure
  | DeleteDataBagItem
  | DeleteDataBagItemSuccess
  | DeleteDataBagItemFailure
  | UpdateDataBagItem
  | UpdateDataBagItemSuccess
  | UpdateDataBagItemFailure
  | CreateDataBagItem
  | CreateDataBagItemSuccess
  | CreateDataBagItemFailure;
