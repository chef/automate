import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { DataBag } from './data-bags.model';

export enum DataBagActionTypes {
  GET_ALL = 'DATA_BAGS::GET_ALL',
  GET_ALL_SUCCESS = 'DATA_BAGS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'DATA_BAGS::GET_ALL::FAILURE',
  CREATE          = 'DATA_BAGS::CREATE',
  CREATE_SUCCESS  = 'DATA_BAGS::CREATE::SUCCESS',
  CREATE_FAILURE  = 'DATA_BAGS::CREATE::FAILURE',
  DELETE          = 'DATA_BAGS::DELETE',
  DELETE_SUCCESS  = 'DATA_BAGS::DELETE::SUCCESS',
  DELETE_FAILURE  = 'DATA_BAGS::DELETE::FAILURE'
}

export interface DataBagsSuccessPayload {
  data_bags: DataBag[];
}

export class GetDataBags implements Action {
  readonly type = DataBagActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string }) { }
}

export class GetDataBagsSuccess implements Action {
  readonly type = DataBagActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: DataBagsSuccessPayload) { }
}

export class GetDataBagsFailure implements Action {
  readonly type = DataBagActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class CreateDataBag implements Action {
  readonly type = DataBagActionTypes.CREATE;
  constructor(public payload: { dataBag: DataBag }) { }
}

export class CreateDataBagSuccess implements Action {
  readonly type = DataBagActionTypes.CREATE_SUCCESS;
  constructor(public payload: { databag: DataBag }) { }
}

export class CreateDataBagFailure implements Action {
  readonly type = DataBagActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteDataBag implements Action {
  readonly type = DataBagActionTypes.DELETE;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class DeleteDataBagSuccess implements Action {
  readonly type = DataBagActionTypes.DELETE_SUCCESS;
  constructor(public payload: { name: string }) { }
}

export class DeleteDataBagFailure implements Action {
  readonly type = DataBagActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DataBagActions =
  | GetDataBags
  | GetDataBagsSuccess
  | GetDataBagsFailure
  | CreateDataBag
  | CreateDataBagSuccess
  | CreateDataBagFailure
  | DeleteDataBag
  | DeleteDataBagSuccess
  | DeleteDataBagFailure;
