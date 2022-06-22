import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { NodeCredential, NodeCredentialTypes, KVData } from './node-credential.model';

export enum NodeCredentialActionTypes {
  SEARCH                = 'NODECREDENTIAL::SEARCH',
  SEARCH_SUCCESS        = 'NODECREDENTIAL::SEARCH::SUCCESS',
  SEARCH_FAILURE        = 'NODECREDENTIAL::SEARCH::FAILURE',
  GET_ALL               = 'NODECREDENTIAL::GET_ALL',
  GET_ALL_SUCCESS       = 'NODECREDENTIAL::GET_ALL::SUCCESS',
  GET_ALL_FAILURE       = 'NODECREDENTIAL::GET_ALL::FAILURE',
  GET                   = 'NODECREDENTIAL::GET',
  GET_SUCCESS           = 'NODECREDENTIAL::GET::SUCCESS',
  GET_FAILURE           = 'NODECREDENTIAL::GET::FAILURE',
  CREATE                = 'NODECREDENTIAL::CREATE',
  CREATE_SUCCESS        = 'NODECREDENTIAL::CREATE::SUCCESS',
  CREATE_FAILURE        = 'NODECREDENTIAL::CREATE::FAILURE',
  UPDATE                = 'NODECREDENTIAL::UPDATE',
  UPDATE_SUCCESS        = 'NODECREDENTIAL::UPDATE::SUCCESS',
  UPDATE_FAILURE        = 'NODECREDENTIAL::UPDATE::FAILURE',
  DELETE                = 'NODECREDENTIAL::DELETE',
  DELETE_SUCCESS        = 'NODECREDENTIAL::DELETE::SUCCESS',
  DELETE_FAILURE        = 'NODECREDENTIAL::DELETE::FAILURE',
  RESET                 = 'NODECREDENTIAL::RESET'

}

export interface NodeCredentialSuccessPayload {
  nodeCredential: NodeCredential;
}

export interface NodeCredentialsSuccessPayload {
  nodeCredentials: NodeCredential[];
}

export interface NodeCredentialsSearchPayload {
  filters?: any[];
  page?: number;
  per_page?: number;
  sort?: string;
  order?: string;
}
export class NodeCredentialsSearch implements Action {
  readonly type = NodeCredentialActionTypes.SEARCH;
  constructor(public payload: NodeCredentialsSearchPayload) {}
}

export interface NodeCredentialsSearchSuccessPayload {
  secrets: NodeCredential[];
  total?: number;
}
export class NodeCredentialsSearchSuccess implements Action {
  readonly type = NodeCredentialActionTypes.SEARCH_SUCCESS;
  constructor(public payload: NodeCredentialsSearchSuccessPayload) {}
}

export class NodeCredentialsSearchFailure implements Action {
  readonly type = NodeCredentialActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}


export class GetNodeCredentials implements Action {
  readonly type = NodeCredentialActionTypes.GET_ALL;
  constructor(public payload: {}) { }
}

export class GetNodeCredentialsSuccess implements Action {
  readonly type = NodeCredentialActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: NodeCredentialsSuccessPayload) { }
}

export class GetNodeCredentialsFailure implements Action {
  readonly type = NodeCredentialActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetNodeCredential implements Action {
  readonly type = NodeCredentialActionTypes.GET;
  constructor(public payload: { id: string }) { }
}

export class GetNodeCredentialSuccess implements Action {
  readonly type = NodeCredentialActionTypes.GET_SUCCESS;
  constructor(public payload: NodeCredential) { }
}

export class GetNodeCredentialFailure implements Action {
  readonly type = NodeCredentialActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}


export class UpdateNodeCredential implements Action {
  readonly type = NodeCredentialActionTypes.UPDATE;

  constructor(public payload: NodeCredential ) { }
}

export class UpdateNodeCredentialSuccess implements Action {
  readonly type = NodeCredentialActionTypes.UPDATE_SUCCESS;

  constructor(public payload: NodeCredential) { }
}

export class UpdateNodeCredentialFailure implements Action {
  readonly type = NodeCredentialActionTypes.UPDATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export interface CreateNodeCredentialPayload {
  id?: string;
  name: string;
  type: NodeCredentialTypes;
  last_modified?: string;
  tags: Array<KVData>;
  data: Array<KVData>;
}

export class CreateNodeCredential implements Action {
  readonly type = NodeCredentialActionTypes.CREATE;
  constructor(public payload: CreateNodeCredentialPayload) { }
}

export class CreateNodeCredentialSuccess implements Action {
  readonly type = NodeCredentialActionTypes.CREATE_SUCCESS;
  constructor(public payload) { }
}

export class CreateNodeCredentialFailure implements Action {
  readonly type = NodeCredentialActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export interface NavManagerListPayload {
  sort: string;
  order: string;
}

export class DeleteNodeCredential implements Action {
  readonly type = NodeCredentialActionTypes.DELETE;
  constructor(public payload: NodeCredential) { }
}

export class DeleteNodeCredentialSuccess implements Action {
  readonly type = NodeCredentialActionTypes.DELETE_SUCCESS;
  constructor(public payload: NodeCredential) { }
}

export class DeleteNodeCredentialFailure implements Action {
  readonly type = NodeCredentialActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class ResetStore implements Action {
  readonly type = NodeCredentialActionTypes.RESET;
  constructor() { }
}


export type NodeCredentialActions =
  NodeCredentialsSearch
  | NodeCredentialsSearchSuccess
  | NodeCredentialsSearchFailure
  | GetNodeCredentials
  | GetNodeCredentialsFailure
  | GetNodeCredentialsSuccess
  | GetNodeCredential
  | GetNodeCredentialSuccess
  | GetNodeCredentialFailure
  | CreateNodeCredential
  | CreateNodeCredentialSuccess
  | CreateNodeCredentialFailure
  | UpdateNodeCredential
  | UpdateNodeCredentialSuccess
  | UpdateNodeCredentialFailure
  | DeleteNodeCredential
  | DeleteNodeCredentialSuccess
  | DeleteNodeCredentialFailure
  | ResetStore;
