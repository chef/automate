import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { PolicyFile, IncludedPolicyLocks } from './policy-file.model';

export enum PolicyFileActionTypes {
  GET_ALL            = 'POLICYFILES::GET_ALL',
  GET_ALL_SUCCESS    = 'POLICYFILES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE    = 'POLICYFILES::GET_ALL::FAILURE',
  DELETE             = 'POLICYFILES::DELETE',
  DELETE_SUCCESS     = 'POLICYFILES::DELETE::SUCCESS',
  DELETE_FAILURE     = 'POLICYFILES::DELETE::FAILURE',
  GET                = 'POLICYFILES::GET',
  GET_SUCCESS        = 'POLICYFILES::GET::SUCCESS',
  GET_FAILURE        = 'POLICYFILES::GET::FAILURE',
  GET_GROUPS         = 'POLICYFILES::GET_GROUPS',
  GET_GROUPS_SUCCESS = 'POLICYFILES::GET_GROUPS::SUCCESS',
  GET_GROUPS_FAILURE = 'POLICYFILES::GET_GROUPS::FAILURE',
  GET_GROUP          = 'POLICYFILES::GET_GROUP',
  GET_GROUP_SUCCESS  = 'POLICYFILES::GET_GROUP::SUCCESS',
  GET_GROUP_FAILURE  = 'POLICYFILES::GET_GROUP::FAILURE'
}

export interface PolicyFilesSuccessPayload {
  policies: PolicyFile[];
}

export interface PolicyGroupSuccessPayload {
  name: string;
  policies: IncludedPolicyLocks[];
  uri: string;
}

export class GetPolicyFiles implements Action {
  readonly type = PolicyFileActionTypes.GET_ALL;
  constructor(public payload: { server_id: string, org_id: string }) { }
}

export class GetPolicyFilesSuccess implements Action {
  readonly type = PolicyFileActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: PolicyFilesSuccessPayload) { }
}

export class GetPolicyFilesFailure implements Action {
  readonly type = PolicyFileActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeletePolicyFile implements Action {
  readonly type = PolicyFileActionTypes.DELETE;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class DeletePolicyFileSuccess implements Action {
  readonly type = PolicyFileActionTypes.DELETE_SUCCESS;
  constructor(public payload: { name: string }) { }
}

export class DeletePolicyFileFailure implements Action {
  readonly type = PolicyFileActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetPolicyFile implements Action {
  readonly type = PolicyFileActionTypes.GET;
  constructor(public payload: {
    server_id: string, org_id: string, name: string, revision: string }) { }
}

export class GetPolicyFileSuccess implements Action {
  readonly type = PolicyFileActionTypes.GET_SUCCESS;
  constructor(public payload: PolicyFile) { }
}

export class GetPolicyFileFailure implements Action {
  readonly type = PolicyFileActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetPolicyGroups implements Action {
  readonly type = PolicyFileActionTypes.GET_GROUPS;
  constructor(public payload: { server_id: string, org_id: string }) { }
}

export class GetPolicyGroupsSuccess implements Action {
  readonly type = PolicyFileActionTypes.GET_GROUPS_SUCCESS;
  constructor(public payload: PolicyFilesSuccessPayload) { }
}

export class GetPolicyGroupsFailure implements Action {
  readonly type = PolicyFileActionTypes.GET_GROUPS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetPolicyGroup implements Action {
  readonly type = PolicyFileActionTypes.GET_GROUP;
  constructor(public payload: { server_id: string, org_id: string, name: string }) { }
}

export class GetPolicyGroupSuccess implements Action {
  readonly type = PolicyFileActionTypes.GET_GROUP_SUCCESS;
  constructor(public payload: PolicyGroupSuccessPayload) { }
}

export class GetPolicyGroupFailure implements Action {
  readonly type = PolicyFileActionTypes.GET_GROUP_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type PolicyFileActions =
  | GetPolicyFiles
  | GetPolicyFilesSuccess
  | GetPolicyFilesFailure
  | DeletePolicyFile
  | DeletePolicyFileSuccess
  | DeletePolicyFileFailure
  | GetPolicyFile
  | GetPolicyFileSuccess
  | GetPolicyFileFailure
  | GetPolicyGroups
  | GetPolicyGroupsSuccess
  | GetPolicyGroupsFailure
  | GetPolicyGroup
  | GetPolicyGroupSuccess
  | GetPolicyGroupFailure;
