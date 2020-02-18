import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { Org } from './org.model';

export enum OrgActionTypes {
  GET_ALL = 'ORGS::GET_ALL',
  GET_ALL_SUCCESS = 'ORGS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE = 'ORGS::GET_ALL::FAILURE',
  GET = 'ORGS::GET',
  GET_SUCCESS = 'ORGS::GET::SUCCESS',
  GET_FAILURE = 'ORGS::GET::FAILURE',
  CREATE = 'ORGS::CREATE',
  CREATE_SUCCESS = 'ORGS::CREATE::SUCCESS',
  CREATE_FAILURE = 'ORGS::CREATE::FAILURE',
  DELETE = 'ORGS::DELETE',
  DELETE_SUCCESS = 'ORGS::DELETE::SUCCESS',
  DELETE_FAILURE = 'ORGS::DELETE::FAILURE',
  UPDATE = 'ORGS::UPDATE',
  UPDATE_SUCCESS = 'ORGS::UPDATE::SUCCESS',
  UPDATE_FAILURE = 'ORGS::UPDATE::FAILURE'
}

export interface OrgSuccessPayload {
  org: Org;
}

export class GetOrgs implements Action {
  readonly type = OrgActionTypes.GET_ALL;

  constructor(public payload: { server_id: string }) { }
}

export interface OrgsSuccessPayload {
  orgs: Org[];
}

export class GetOrgsSuccess implements Action {
  readonly type = OrgActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: OrgsSuccessPayload) { }
}

export class GetOrgsFailure implements Action {
  readonly type = OrgActionTypes.GET_ALL_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetOrg implements Action {
  readonly type = OrgActionTypes.GET;

  constructor(public payload: { server_id: string, id: string }) { }
}

export class GetOrgSuccess implements Action {
  readonly type = OrgActionTypes.GET_SUCCESS;

  constructor(public payload: OrgSuccessPayload) { }
}

export class GetOrgFailure implements Action {
  readonly type = OrgActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export interface CreateOrgPayload {
  server_id: string;
  name: string;
  admin_user: string;
  admin_key: string;
}

export class CreateOrg implements Action {
  readonly type = OrgActionTypes.CREATE;
  constructor(public payload:  CreateOrgPayload ) { }
}

export class CreateOrgSuccess implements Action {
  readonly type = OrgActionTypes.CREATE_SUCCESS;
  constructor(public payload: OrgSuccessPayload) { }
}

export class CreateOrgFailure implements Action {
  readonly type = OrgActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteOrg implements Action {
  readonly type = OrgActionTypes.DELETE;

  constructor(public payload: { server_id: string, id: string, name: string }) { }
}

export class DeleteOrgSuccess implements Action {
  readonly type = OrgActionTypes.DELETE_SUCCESS;

  constructor(public payload: { id: string, name: string }) { }
}

export class DeleteOrgFailure implements Action {
  readonly type = OrgActionTypes.DELETE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateOrg implements Action {
  readonly type = OrgActionTypes.UPDATE;

  constructor(public payload: { org: Org }) { }
}

export class UpdateOrgSuccess implements Action {
  readonly type = OrgActionTypes.UPDATE_SUCCESS;

  constructor(public payload: OrgSuccessPayload) { }
}

export class UpdateOrgFailure implements Action {
  readonly type = OrgActionTypes.UPDATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type OrgActions =
  | GetOrgs
  | GetOrgsSuccess
  | GetOrgsFailure
  | GetOrg
  | GetOrgSuccess
  | GetOrgFailure
  | CreateOrg
  | CreateOrgSuccess
  | CreateOrgFailure
  | DeleteOrg
  | DeleteOrgSuccess
  | DeleteOrgFailure
  | UpdateOrg
  | UpdateOrgSuccess
  | UpdateOrgFailure;
