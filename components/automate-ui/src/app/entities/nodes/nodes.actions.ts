import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

export enum NodesActionTypes {
  LIST_NODES             = 'NODES::LIST_NODES',
  LIST_NODES_SUCCESS     = 'NODES::LIST_NODES::SUCCESS',
  LIST_NODES_FAILURE     = 'NODES::LIST_NODES::FAILURE'
}
export interface NodesAction extends Action {
  payload?: any;
}

export const GET_NODES = 'GET_NODES';
export const getNodes = (payload): NodesAction => ({ type: NodesActionTypes.LIST_NODES, payload });

export const DELETE_NODE = 'DELETE_NODE';
export const deleteNode = (payload): NodesAction => ({ type: DELETE_NODE, payload });

export const DELETE_NODE_SUCCESS = 'DELETE_NODE_SUCCESS';
export const deleteNodeSuccess = (payload): NodesAction => {
  return { type: DELETE_NODE_SUCCESS, payload };
};

export interface SearchNodesPayload {
  filters?: any[];
  page?: number;
  per_page?: number;
  sort?: string;
  order?: string;
}

export class ListNodes implements Action {
  readonly type = NodesActionTypes.LIST_NODES;
  constructor(public payload: SearchNodesPayload) {}
}

export interface ListNodesSuccessPayload {
  nodes: any;
  total: number;
  total_reachable: number;
  total_unreachable: number;
  total_unknown: number;
}
export class ListNodesSuccess implements Action {
  readonly type = NodesActionTypes.LIST_NODES_SUCCESS;
  constructor(public payload: ListNodesSuccessPayload) {}
}

export class ListNodesFailure implements Action {
  readonly type = NodesActionTypes.LIST_NODES_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export type NodesActions =
  | ListNodes
  | ListNodesSuccess
  | ListNodesFailure;
