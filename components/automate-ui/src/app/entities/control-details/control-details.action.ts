import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { ControlDetail, Filters } from './control-details.model';

export enum ControlDetailActionTypes {
  GET = 'CONTROLDETAIL::GET',
  GET_SUCCESS = 'CONTROLDETAIL::GET::SUCCESS',
  GET_FAILURE = 'CONTROLDETAIL::GET::FAILURE',
}

export interface ControlDetailSuccessPayload {
  controlDetail: ControlDetail[];
}

export class GetControlDetail implements Action {
  readonly type = ControlDetailActionTypes.GET;
  constructor(public payload: Filters ) { }
}

export class GetControlDetailSuccess implements Action {
  readonly type = ControlDetailActionTypes.GET_SUCCESS;
  constructor(public payload: ControlDetailSuccessPayload) { }
}

export class GetControlDetailFailure implements Action {
  readonly type = ControlDetailActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type ControlDetailActions =
  | GetControlDetail
  | GetControlDetailSuccess
  | GetControlDetailFailure;
