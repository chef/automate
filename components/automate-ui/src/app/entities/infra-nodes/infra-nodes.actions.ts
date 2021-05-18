import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { InfraNode } from './infra-nodes.model';

export enum NodeActionTypes {
  GET_ALL                     = 'NODES::GET_ALL',
  GET_ALL_SUCCESS             = 'NODES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE             = 'NODES::GET_ALL::FAILURE',
  GET                         = 'NODES::GET',
  GET_SUCCESS                 = 'NODES::GET::SUCCESS',
  GET_FAILURE                 = 'NODES::GET::FAILURE',
  UPDATE_ENVIRONMENT          = 'NODES::ENVIRONMENT::UPDATE',
  UPDATE_ENVIRONMENT_SUCCESS  = 'NODES::ENVIRONMENT::UPDATE::SUCCESS',
  UPDATE_ENVIRONMENT_FAILURE  = 'NODES::ENVIRONMENT::UPDATE::FAILURE',
  UPDATE_TAGS                 = 'NODES::TAGS::UPDATE',
  UPDATE_TAGS_SUCCESS         = 'NODES::TAGS::UPDATE::SUCCESS',
  UPDATE_TAGS_FAILURE         = 'NODES::TAGS::UPDATE::FAILURE',
  DELETE                      = 'NODES::DELETE',
  DELETE_SUCCESS              = 'NODES::DELETE::SUCCESS',
  DELETE_FAILURE              = 'NODES::DELETE::FAILURE'
}

export interface NodesSuccessPayload {
  nodes: InfraNode[];
  total: number;
}

export interface NodesPayload {
  nodeName: string;
  server_id: string;
  org_id: string;
  page: number;
  per_page: number;
}

export interface UpdateNodeEnvPayload {
  server_id: string;
  org_id: string;
  name: string;
  environment: string;
}

export interface UpdateNodeTagPayload {
  server_id: string;
  org_id: string;
  name: string;
  action: string;
  tags: string[];
}

export class GetNodes implements Action {
  readonly type = NodeActionTypes.GET_ALL;
  constructor(public payload: NodesPayload) { }
}
export class GetNodesSuccess implements Action {
  readonly type = NodeActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: NodesSuccessPayload) { }
}
export class GetNodesFailure implements Action {
  readonly type = NodeActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetNode implements Action {
  readonly type = NodeActionTypes.GET;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetNodeSuccess implements Action {
  readonly type = NodeActionTypes.GET_SUCCESS;
  constructor(public payload: InfraNode) { }
}

export class GetNodeFailure implements Action {
  readonly type = NodeActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteNode implements Action {
  readonly type = NodeActionTypes.DELETE;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class DeleteNodeSuccess implements Action {
  readonly type = NodeActionTypes.DELETE_SUCCESS;
  constructor(public payload: { name: string }) { }
}

export class DeleteNodeFailure implements Action {
  readonly type = NodeActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateNodeEnvironment implements Action {
  readonly type = NodeActionTypes.UPDATE_ENVIRONMENT;
  constructor(public payload: { node:  UpdateNodeEnvPayload}) { }
}

export class UpdateNodeEnvironmentSuccess implements Action {
  readonly type = NodeActionTypes.UPDATE_ENVIRONMENT_SUCCESS;
  constructor(public payload: {name: string, environment: string}) { }
}

export class UpdateNodeEnvironmentFailure implements Action {
  readonly type = NodeActionTypes.UPDATE_ENVIRONMENT_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateNodeTags implements Action {
  readonly type = NodeActionTypes.UPDATE_TAGS;
  constructor(public payload: { node:  UpdateNodeTagPayload}) { }
}

export class UpdateNodeTagsSuccess implements Action {
  readonly type = NodeActionTypes.UPDATE_TAGS_SUCCESS;
  constructor(public payload: {tags: string[]}) { }
}

export class UpdateNodeTagsFailure implements Action {
  readonly type = NodeActionTypes.UPDATE_TAGS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type NodeActions =
  | GetNodes
  | GetNodesSuccess
  | GetNodesFailure
  | GetNode
  | GetNodeSuccess
  | GetNodeFailure
  | DeleteNode
  | DeleteNodeSuccess
  | DeleteNodeFailure
  | UpdateNodeEnvironment
  | UpdateNodeEnvironmentSuccess
  | UpdateNodeEnvironmentFailure
  | UpdateNodeTags
  | UpdateNodeTagsSuccess
  | UpdateNodeTagsFailure;
