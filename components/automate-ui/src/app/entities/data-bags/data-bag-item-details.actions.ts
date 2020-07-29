import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { DataBagsItemDetails } from './data-bags.model';

export enum DataBagItemDetailsActionTypes {
  GET = 'DATA_BAG_ITEM_DETAILS::GET',
  GET_SUCCESS = 'DATA_BAG_ITEM_DETAILS::GET::SUCCESS',
  GET_FAILURE = 'DATA_BAG_ITEM_DETAILS::GET::FAILURE'
}

export class GetDataBagItemDetails implements Action {
  readonly type = DataBagItemDetailsActionTypes.GET;
  constructor(public payload: {
    server_id: string,
    org_id: string,
    name: string,
    item_name: string
  }) { }
}

export class GetDataBagItemDetailsSuccess implements Action {
  readonly type = DataBagItemDetailsActionTypes.GET_SUCCESS;
  constructor(public payload: DataBagsItemDetails) { }
}

export class GetDataBagItemDetailsFailure implements Action {
  readonly type = DataBagItemDetailsActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DataBagItemDetailsActions =
  | GetDataBagItemDetails
  | GetDataBagItemDetailsSuccess
  | GetDataBagItemDetailsFailure;
