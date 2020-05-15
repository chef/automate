import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { AdminKey } from './reset-admin-key.model';

export enum AdminKeyActionTypes {
  UPDATE = 'ADMINKEY::UPDATE',
  UPDATE_SUCCESS = 'ADMINKEY::UPDATE::SUCCESS',
  UPDATE_FAILURE = 'ADMINKEY::UPDATE::FAILURE'
}

export interface AdminKeySuccessPayload {
  adminKey: AdminKey;
}

export class UpdateAdminKey implements Action {
  readonly type = AdminKeyActionTypes.UPDATE;

  constructor(public payload: {server_id: string, org_id: string, admin_Key: AdminKey} ) { }
}

export class UpdateAdminKeySuccess implements Action {
  readonly type = AdminKeyActionTypes.UPDATE_SUCCESS;

  constructor(public payload) { }
}

export class UpdateAdminKeyFailure implements Action {
  readonly type = AdminKeyActionTypes.UPDATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type AdminKeyActions =
  | UpdateAdminKey
  | UpdateAdminKeySuccess
  | UpdateAdminKeyFailure;
