import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { DataBags } from './data-bags.model';

export enum DataBagDetailsActionTypes {
  GET_ALL = 'DATA_BAG_DETAILS::GET_ALL',
  GET_ALL_SUCCESS = 'DATA_BAG_DETAILS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'DATA_BAG_DETAILS::GET_ALL::FAILURE'
}

export interface DataBagDetailsSuccessPayload {
  data_bags: DataBags[];
}

export class GetDataBagDetails implements Action {
  readonly type = DataBagDetailsActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetDataBagDetailsSuccess implements Action {
  readonly type = DataBagDetailsActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: DataBagDetailsSuccessPayload) { }
}

export class GetDataBagDetailsFailure implements Action {
  readonly type = DataBagDetailsActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DataBagDetailsActions =
  | GetDataBagDetails
  | GetDataBagDetailsSuccess
  | GetDataBagDetailsFailure;
