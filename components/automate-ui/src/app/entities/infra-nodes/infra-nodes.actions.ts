import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { InfraNode } from './infra-nodes.model';

export enum NodeActionTypes {
  GET_ALL = 'NODES::GET_ALL',
  GET_ALL_SUCCESS = 'NODES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'NODES::GET_ALL::FAILURE'
}

export class GetNodes implements Action {
  readonly type = NodeActionTypes.GET_ALL;

  constructor(public payload: { server_id: string, org_id: string }) { }
}

export interface NodesSuccessPayload {
  nodes: InfraNode[];
}

export class GetNodesSuccess implements Action {
  readonly type = NodeActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: NodesSuccessPayload) { }
}

export class GetNodesFailure implements Action {
  readonly type = NodeActionTypes.GET_ALL_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type NodeActions =
  | GetNodes
  | GetNodesSuccess
  | GetNodesFailure;
