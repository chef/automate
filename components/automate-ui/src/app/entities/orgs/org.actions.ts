import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { Org, UploadFile, PreviewData } from './org.model';

export enum OrgActionTypes {
  GET_ALL                  = 'ORGS::GET_ALL',
  GET_ALL_SUCCESS          = 'ORGS::GET_ALL::SUCCESS',
  GET_ALL_FAILURE          = 'ORGS::GET_ALL::FAILURE',
  GET                      = 'ORGS::GET',
  GET_SUCCESS              = 'ORGS::GET::SUCCESS',
  GET_FAILURE              = 'ORGS::GET::FAILURE',
  CREATE                   = 'ORGS::CREATE',
  CREATE_SUCCESS           = 'ORGS::CREATE::SUCCESS',
  CREATE_FAILURE           = 'ORGS::CREATE::FAILURE',
  DELETE                   = 'ORGS::DELETE',
  DELETE_SUCCESS           = 'ORGS::DELETE::SUCCESS',
  DELETE_FAILURE           = 'ORGS::DELETE::FAILURE',
  UPDATE                   = 'ORGS::UPDATE',
  UPDATE_SUCCESS           = 'ORGS::UPDATE::SUCCESS',
  UPDATE_FAILURE           = 'ORGS::UPDATE::FAILURE',
  UPLOAD                   = 'ORGS::UPLOAD',
  UPLOAD_SUCCESS           = 'ORGS::UPLOAD::SUCCESS',
  UPLOAD_FAILURE           = 'ORGS::UPLOAD::FAILURE',
  CANCEL_MIGRATION         = 'ORGS::CANCEL::MIGRATION',
  CANCEL_MIGRATION_SUCCESS = 'ORGS::CANCEL::MIGRATION::SUCCESS',
  CANCEL_MIGRATION_FAILURE = 'ORGS::CANCEL::MIGRATION::FAILURE',
  GET_PREVIEW_DATA         = 'ORGS::GET_PREVIEW_DATA',
  GET_PREVIEW_DATA_SUCCESS = 'ORGS::GET_PREVIEW_DATA::SUCCESS',
  GET_PREVIEW_DATA_FAILURE = 'ORGS::GET_PREVIEW_DATA::FAILURE',
  CONFIRM_PREVIEW          = 'ORGS::CONFIRM_PREVIEW',
  CONFIRM_PREVIEW_SUCCESS  = 'ORGS::CONFIRM_PREVIEW::SUCCESS',
  CONFIRM_PREVIEW_FAILURE  = 'ORGS::CONFIRM_PREVIEW::FAILURE',
  CHECK_USER               = 'ORGS::CHECK_USER',
  CHECK_USER_SUCCESS       = 'ORGS::CHECK_USER::SUCCESS',
  CHECK_USER_FAILURE       = 'ORGS::CHECK_USER::FAILURE'
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
  id: string;
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

export interface UploadSuccessPayload {
  success: boolean;
  migration_id: string;
}

export class UploadZip implements Action {
  readonly type = OrgActionTypes.UPLOAD;

  constructor(public payload: UploadFile) { }
}

export class UploadZipSuccess implements Action {
  readonly type = OrgActionTypes.UPLOAD_SUCCESS;

  constructor(public payload: UploadSuccessPayload) { }
}

export class UploadZipFailure implements Action {
  readonly type = OrgActionTypes.UPLOAD_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export interface CancelSuccessPayload {
  success: boolean;
  error: [];
}

export class CancelMigration implements Action {
  readonly type = OrgActionTypes.CANCEL_MIGRATION;

  constructor(public payload: { server_id: string, migration_id: string}) { }
}

export class CancelMigrationSuccess implements Action {
  readonly type = OrgActionTypes.CANCEL_MIGRATION_SUCCESS;

  constructor(public payload: CancelSuccessPayload) { }
}

export class CancelMigrationFailure implements Action {
  readonly type = OrgActionTypes.CANCEL_MIGRATION_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export interface PreviewSuccessPayload {
  staged_data: any;
  PreviewData: PreviewData[];
}

export class GetPreviewData implements Action {
  readonly type = OrgActionTypes.GET_PREVIEW_DATA;

  constructor(public payload: { migration_id: string}) {}
}

export class GetPreviewDataSuccess implements Action {
  readonly type = OrgActionTypes.GET_PREVIEW_DATA_SUCCESS;

  constructor(public payload: PreviewSuccessPayload) {}
}

export class GetPreviewDataFailure implements Action {
  readonly type = OrgActionTypes.GET_PREVIEW_DATA_FAILURE;

  constructor(public payload: HttpErrorResponse) {}
}

export interface ConfirmSuccessPayload {
  migration_id: string;
}

export class ConfirmPreview implements Action {
  readonly type = OrgActionTypes.CONFIRM_PREVIEW;

  constructor(public payload: { server_id: string, previewData: PreviewData}) { }
}

export class ConfirmPreviewSuccess implements Action {
  readonly type = OrgActionTypes.CONFIRM_PREVIEW_SUCCESS;

  constructor(public payload: ConfirmSuccessPayload) { }
}

export class ConfirmPreviewFailure implements Action {
  readonly type = OrgActionTypes.CONFIRM_PREVIEW_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export interface CheckUserPayload {
  user: {
    name: string;
    id: string;
    membership_id: string;
  };
}

export class CheckUser implements Action {
  readonly type = OrgActionTypes.CHECK_USER;

  constructor(public payload: { user: string}) { }
}

export class CheckUserSuccess implements Action {
  readonly type = OrgActionTypes.CHECK_USER_SUCCESS;

  constructor(public paylode: CheckUserPayload) { }
}

export class CheckUserFailure implements Action {
  readonly type = OrgActionTypes.CHECK_USER_FAILURE;

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
  | UpdateOrgFailure
  | UploadZip
  | UploadZipSuccess
  | UploadZipFailure
  | CancelMigration
  | CancelMigrationSuccess
  | CancelMigrationFailure
  | GetPreviewData
  | GetPreviewDataSuccess
  | GetPreviewDataFailure
  | ConfirmPreview
  | ConfirmPreviewSuccess
  | ConfirmPreviewFailure
  | CheckUser
  | CheckUserSuccess
  | CheckUserFailure;
