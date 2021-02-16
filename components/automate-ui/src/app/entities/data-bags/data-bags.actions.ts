import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { DataBags } from './data-bags.model';

export enum DataBagsActionTypes {
  GET_ALL = 'DATA_BAGS::GET_ALL',
  GET_ALL_SUCCESS = 'DATA_BAGS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'DATA_BAGS::GET_ALL::FAILURE'
}

export interface DataBagsSuccessPayload {
  data_bags: DataBags[];
}

export class GetDataBags implements Action {
  readonly type = DataBagsActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string }) { }
}

export class GetDataBagsSuccess implements Action {
  readonly type = DataBagsActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: DataBagsSuccessPayload) { }
}

export class GetDataBagsFailure implements Action {
  readonly type = DataBagsActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DataBagsActions =
  | GetDataBags
  | GetDataBagsSuccess
  | GetDataBagsFailure;
