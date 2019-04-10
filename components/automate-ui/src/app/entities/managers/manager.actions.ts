import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { Manager } from './manager.model';

export enum ManagerActionTypes {
  SEARCH                = 'MANAGER::SEARCH',
  SEARCH_SUCCESS        = 'MANAGER::SEARCH::SUCCESS',
  SEARCH_FAILURE        = 'MANAGER::SEARCH::FAILURE',
  GET_NODES             = 'MANAGER::GET_NODES',
  GET_NODES_SUCCESS     = 'MANAGER::GET_NODES::SUCCESS',
  GET_NODES_FAILURE     = 'MANAGER::GET_NODES::FAILURE',
  SEARCH_NODES          = 'MANAGER::SEARCH_NODES',
  SEARCH_NODES_SUCCESS  = 'MANAGER::SEARCH_NODES::SUCCESS',
  SEARCH_NODES_FAILURE  = 'MANAGER::SEARCH_NODES::FAILURE',
  ALL_NODES             = 'MANAGER::ALL_NODES',
  ALL_NODES_SUCCESS     = 'MANAGER::ALL_NODES::SUCCESS',
  ALL_NODES_FAILURE     = 'MANAGER::ALL_NODES::FAILURE',
  SEARCH_FIELDS         = 'MANAGER::SEARCH_FIELDS',
  SEARCH_FIELDS_SUCCESS = 'MANAGER::SEARCH_FIELDS::SUCCESS',
  SEARCH_FIELDS_FAILURE = 'MANAGER::SEARCH_FIELDS::FAILURE',
  GET                   = 'MANAGER::GET',
  GET_SUCCESS           = 'MANAGER::GET::SUCCESS',
  GET_FAILURE           = 'MANAGER::GET::FAILURE',
  CREATE                = 'MANAGER::CREATE',
  CREATE_SUCCESS        = 'MANAGER::CREATE::SUCCESS',
  CREATE_FAILURE        = 'MANAGER::CREATE::FAILURE',
  DELETE                = 'MANAGER::DELETE',
  DELETE_SUCCESS        = 'MANAGER::DELETE::SUCCESS',
  DELETE_FAILURE        = 'MANAGER::DELETE::FAILURE',
  UPDATE                = 'MANAGER::UPDATE',
  UPDATE_SUCCESS        = 'MANAGER::UPDATE::SUCCESS',
  UPDATE_FAILURE        = 'MANAGER::UPDATE::FAILURE',
  NAV_LIST              = 'MANAGER::NAV::LIST',
  NAV_DETAIL            = 'MANAGER::NAV::DETAIL',
  NAV_EDIT              = 'MANAGER::NAV::EDIT'
}

export interface ManagersSearchPayload {
  filters?: any[];
  page?: number;
  per_page?: number;
  sort?: string;
  order?: string;
}
export class ManagersSearch implements Action {
  readonly type = ManagerActionTypes.SEARCH;
  constructor(public payload: ManagersSearchPayload) {}
}

export interface ManagersSearchSuccessPayload {
  managers: Manager[];
  total: number;
}
export class ManagersSearchSuccess implements Action {
  readonly type = ManagerActionTypes.SEARCH_SUCCESS;
  constructor(public payload: ManagersSearchSuccessPayload) {}
}

export class ManagersSearchFailure implements Action {
  readonly type = ManagerActionTypes.SEARCH_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export interface ManagerGetNodesPayload {
  managerId: string;
  page: number;
  per_page: number;
}
export class ManagerGetNodes implements Action {
  readonly type = ManagerActionTypes.GET_NODES;
  constructor(public payload: ManagerGetNodesPayload) {}
}

export interface ManagerGetNodesSuccessPayload {
  managerId: string;
  nodes: any[];
  total: number;
}
export class ManagerGetNodesSuccess implements Action {
  readonly type = ManagerActionTypes.GET_NODES_SUCCESS;
  constructor(public payload: ManagerGetNodesSuccessPayload) {}
}

export class ManagerGetNodesFailure implements Action {
  readonly type = ManagerActionTypes.GET_NODES_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export interface ManagerSearchNodesPayload {
  managerId: string;
  query: any;
}
export class ManagerSearchNodes implements Action {
  readonly type = ManagerActionTypes.SEARCH_NODES;
  constructor(public payload: ManagerSearchNodesPayload) {}
}

export interface ManagerSearchNodesSuccessPayload {
  managerId: string;
  nodes: any[];
  total: number;
}
export class ManagerSearchNodesSuccess implements Action {
  readonly type = ManagerActionTypes.SEARCH_NODES_SUCCESS;
  constructor(public payload: ManagerSearchNodesSuccessPayload) {}
}

export class ManagerSearchNodesFailure implements Action {
  readonly type = ManagerActionTypes.SEARCH_NODES_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export interface ManagerAllNodesPayload {
  managerId: string;
  query: any;
}
export class ManagerAllNodes implements Action {
  readonly type = ManagerActionTypes.ALL_NODES;
  constructor(public payload: ManagerAllNodesPayload) {}
}

export interface ManagerAllNodesSuccessPayload {
  managerId: string;
  nodes: any[];
  total: number;
}
export class ManagerAllNodesSuccess implements Action {
  readonly type = ManagerActionTypes.ALL_NODES_SUCCESS;
  constructor(public payload: ManagerAllNodesSuccessPayload) {}
}

export class ManagerAllNodesFailure implements Action {
  readonly type = ManagerActionTypes.ALL_NODES_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export interface ManagerSearchFieldsPayload {
  managerId: string;
  field: string;
}
export class ManagerSearchFields implements Action {
  readonly type = ManagerActionTypes.SEARCH_FIELDS;
  constructor(public payload: ManagerSearchFieldsPayload) {}
}

export interface ManagerSearchFieldsSuccessPayload {
  managerId: string;
  field: string;
  fields: string[];
}
export class ManagerSearchFieldsSuccess implements Action {
  readonly type = ManagerActionTypes.SEARCH_FIELDS_SUCCESS;
  constructor(public payload: ManagerSearchFieldsSuccessPayload) {}
}

export class ManagerSearchFieldsFailure implements Action {
  readonly type = ManagerActionTypes.SEARCH_FIELDS_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export class GetManager implements Action {
  readonly type = ManagerActionTypes.GET;

  constructor(public payload: {id: string}) {}
}

export class GetManagerSuccess implements Action {
  readonly type = ManagerActionTypes.GET_SUCCESS;

  constructor(public payload: {manager: Manager}) {}
}

export class GetManagerFailure implements Action {
  readonly type = ManagerActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export interface CreateManagerPayload {
  name: string;
  type: string;
  credentialData: {};
  instanceCredentials: { tag_key: string,
                         tag_value: string,
                         credential_ids: string[] }[];
}

export class CreateManager implements Action {
  readonly type = ManagerActionTypes.CREATE;

  constructor(public payload: CreateManagerPayload ) {}
}

export class CreateManagerSuccess implements Action {
  readonly type = ManagerActionTypes.CREATE_SUCCESS;
}

export class CreateManagerFailure implements Action {
  readonly type = ManagerActionTypes.CREATE_FAILURE;

  constructor(public payload: HttpErrorResponse) {}
}

export class DeleteManager implements Action {
  readonly type = ManagerActionTypes.DELETE;

  constructor(public payload: { id: string }) {}
}

export class DeleteManagerSuccess implements Action {
  readonly type = ManagerActionTypes.DELETE_SUCCESS;

  constructor(public payload: { id: string }) {}
}

export class DeleteManagerFailure implements Action {
  readonly type = ManagerActionTypes.DELETE_FAILURE;

  constructor(public payload: HttpErrorResponse) {}
}

export class UpdateManager implements Action {
  readonly type = ManagerActionTypes.UPDATE;

  constructor(public payload: { id: string,
                                name?: string,
                                type?: string,
                                credentialData?: {},
                                instanceCredentials?: object[],
                                credentialId?: string }) {}
}

export class UpdateManagerSuccess implements Action {
  readonly type = ManagerActionTypes.UPDATE_SUCCESS;
}

export class UpdateManagerFailure implements Action {
  readonly type = ManagerActionTypes.UPDATE_FAILURE;
  constructor(public payload: HttpErrorResponse) {}
}

export interface NavManagerListPayload {
  sort: string;
  order: string;
}
export class NavManagerList implements Action {
  readonly type = ManagerActionTypes.NAV_LIST;
  constructor(public payload: NavManagerListPayload) {}
}

export interface NavManagerDetailPayload {
  managerId: string;
  page: number;
  per_page: number;
}
export class NavManagerDetail implements Action {
  readonly type = ManagerActionTypes.NAV_DETAIL;
  constructor(public payload: NavManagerDetailPayload) {}
}

export interface NavManagerEditPayload {
  managerId: string;
}
export class NavManagerEdit implements Action {
  readonly type = ManagerActionTypes.NAV_EDIT;
  constructor(public payload: NavManagerEditPayload) {}
}

export type ManagerActions =
  | ManagersSearch
  | ManagersSearchSuccess
  | ManagersSearchFailure
  | ManagerGetNodes
  | ManagerGetNodesSuccess
  | ManagerGetNodesFailure
  | ManagerSearchNodes
  | ManagerSearchNodesSuccess
  | ManagerAllNodes
  | ManagerAllNodesSuccess
  | ManagerSearchFields
  | ManagerSearchFieldsSuccess
  | GetManager
  | GetManagerSuccess
  | GetManagerFailure
  | CreateManager
  | CreateManagerSuccess
  | CreateManagerFailure
  | DeleteManager
  | DeleteManagerSuccess
  | DeleteManagerFailure
  | UpdateManager
  | UpdateManagerSuccess
  | UpdateManagerFailure
  | NavManagerDetail
  | NavManagerList
  | NavManagerEdit;
