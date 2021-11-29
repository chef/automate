import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { ControlDetail, Filters } from './control-details.model';

export enum ControlDetailsActionTypes {
  GET = 'CONTROLDETAILS::GET',
  GET_SUCCESS = 'CONTROLDETAILS::GET::SUCCESS',
  GET_FAILURE = 'CONTROLDETAILS::GET::FAILURE',
}

export interface ControlDetailsSuccessPayload {
  controlDetails: ControlDetail[];
}

export class GetControlDetails implements Action {
  readonly type = ControlDetailsActionTypes.GET;
  constructor(public payload: Filters ) { }
}

export class GetControlDetailsSuccess implements Action {
  readonly type = ControlDetailsActionTypes.GET_SUCCESS;
  constructor(public payload: ControlDetailsSuccessPayload) { }
}

export class GetControlDetailsFailure implements Action {
  readonly type = ControlDetailsActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type ControlDetailsActions =
  | GetControlDetails
  | GetControlDetailsSuccess
  | GetControlDetailsFailure;
