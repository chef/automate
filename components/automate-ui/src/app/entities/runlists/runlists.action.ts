import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Runlist } from './runlists.model';

export enum RunlistActionTypes {
  GET_ALL          = 'RUNLIST::GET_ALL',
  GET_ALL_SUCCESS  = 'RUNLIST::GET_ALL::SUCCESS',
  GET_ALL_FAILURE  = 'RUNLIST::GET_ALL::FAILURE'
}

export interface RunlistsSuccessPayload {
  runlists: Runlist;
}

export class GetRunlists implements Action {
  readonly type = RunlistActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string, name: string, id: string }) { }
}

export class GetRunlistsSuccess implements Action {
  readonly type = RunlistActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: Runlist) { }
}

export class GetRunlistsFailure implements Action {
  readonly type = RunlistActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type RunlistActions =
  | GetRunlists
  | GetRunlistsSuccess
  | GetRunlistsFailure;
