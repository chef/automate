import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { NodeRunlist } from './nodeRunlists.model';

export enum NodeRunlistActionTypes {
  GET_ALL          = 'NODERUNLIST::GET_ALL',
  GET_ALL_SUCCESS  = 'NODERUNLIST::GET_ALL::SUCCESS',
  GET_ALL_FAILURE  = 'NODERUNLIST::GET_ALL::FAILURE'
}

export interface NodeRunlistsSuccessPayload {
  nodeRunlists: NodeRunlist;
}

export class GetNodeRunlists implements Action {
  readonly type = NodeRunlistActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string, name: string, id: string }) { }
}

export class GetNodeRunlistsSuccess implements Action {
  readonly type = NodeRunlistActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: NodeRunlist) { }
}

export class GetNodeRunlistsFailure implements Action {
  readonly type = NodeRunlistActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type NodeRunlistActions =
  | GetNodeRunlists
  | GetNodeRunlistsSuccess
  | GetNodeRunlistsFailure;
