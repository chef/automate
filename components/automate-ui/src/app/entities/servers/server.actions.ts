import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';

import { MigrationStatus, Server , User, WebUIKey } from './server.model';
import { ValidateWebUIKeyResponse } from './server.requests';

export enum ServerActionTypes {
  GET_ALL                        = 'SERVER::GET_ALL',
  GET_ALL_SUCCESS                = 'SERVER::GET_ALL::SUCCESS',
  GET_ALL_FAILURE                = 'SERVER::GET_ALL::FAILURE',
  GET                            = 'SERVER::GET',
  GET_SUCCESS                    = 'SERVER::GET::SUCCESS',
  GET_FAILURE                    = 'SERVER::GET::FAILURE',
  CREATE                         = 'SERVER::CREATE',
  CREATE_SUCCESS                 = 'SERVER::CREATE::SUCCESS',
  CREATE_FAILURE                 = 'SERVER::CREATE::FAILURE',
  UPDATE                         = 'SERVER::UPDATE',
  UPDATE_SUCCESS                 = 'SERVER::UPDATE::SUCCESS',
  UPDATE_FAILURE                 = 'SERVER::UPDATE::FAILURE',
  DELETE                         = 'SERVER::CREATE::DELETE',
  DELETE_SUCCESS                 = 'SERVER::CREATE::DELETE::SUCCESS',
  DELETE_FAILURE                 = 'SERVER::CREATE::DELETE::FAILURE',
  GET_USERS                      = 'SERVER::GET_USERS',
  GET_USERS_SUCCESS              = 'SERVER::GET_USERS::SUCCESS',
  GET_USERS_FAILURE              = 'SERVER::GET_USERS::FAILURE',
  UPDATE_WEB_UI_KEY              = 'SERVER::UPDATE_WEB_UI_KEY',
  UPDATE_WEB_UI_KEY_SUCCESS      = 'SERVER::UPDATE_WEB_UI_KEY::SUCCESS',
  UPDATE_WEB_UI_KEY_FAILURE      = 'SERVER::UPDATE_WEB_UI_KEY::FAILURE',
  VALIDATE_WEB_UI_KEY            = 'SERVER::VALIDATE_WEB_UI_KEY',
  VALIDATE_WEB_UI_KEY_SUCCESS    = 'SERVER::VALIDATE_WEB_UI_KEY::SUCCESS',
  VALIDATE_WEB_UI_KEY_SUCCESS_NOT= 'SERVER::VALIDATE_WEB_UI_KEY::SUCCESS_NOT',
  VALIDATE_WEB_UI_KEY_FAILURE    = 'SERVER::VALIDATE_WEB_UI_KEY::FAILURE',
  GET_MIGRATION_STATUS           = 'SERVER::GET_MIGRATION_STATUS',
  GET_MIGRATION_STATUS_SUCCESS   = 'SERVER::GET_MIGRATION_STATUS::SUCCESS',
  GET_MIGRATION_STATUS_FAILURE   = 'SERVER::GET_MIGRATION_STATUS::FAILURE'
}


export interface ServerSuccessPayload {
  server: Server;
}

export interface GetServersSuccessPayload {
  servers: Server[];
}

export class GetServers implements Action {
  readonly type = ServerActionTypes.GET_ALL;
}

export class GetServersSuccess implements Action {
  readonly type = ServerActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: GetServersSuccessPayload) { }
}

export class GetServersFailure implements Action {
  readonly type = ServerActionTypes.GET_ALL_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetServer implements Action {
  readonly type = ServerActionTypes.GET;

  constructor(public payload: { id: string }) { }
}

export class GetServerSuccess implements Action {
  readonly type = ServerActionTypes.GET_SUCCESS;

  constructor(public payload: ServerSuccessPayload) { }
}

export class GetServerFailure implements Action {
  readonly type = ServerActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse, public id: string) { }
}

export interface CreateServerPayload {
  id: string;
  name: string;
  fqdn: string;
  ip_address: string;
  webui_key: string;
}

export class CreateServer implements Action {
  readonly type = ServerActionTypes.CREATE;

  constructor(public payload: CreateServerPayload) { }
}

export class CreateServerSuccess implements Action {
  readonly type = ServerActionTypes.CREATE_SUCCESS;

  constructor(public payload: ServerSuccessPayload) { }
}

export class CreateServerFailure implements Action {
  readonly type = ServerActionTypes.CREATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteServer implements Action {
  readonly type = ServerActionTypes.DELETE;

  constructor(public payload: { id: string, name: string }) { }
}

export class DeleteServerSuccess implements Action {
  readonly type = ServerActionTypes.DELETE_SUCCESS;

  constructor(public payload: { id: string, name: string }) { }
}

export class DeleteServerFailure implements Action {
  readonly type = ServerActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateServer implements Action {
  readonly type = ServerActionTypes.UPDATE;

  constructor(public payload: { server: Server }) { }
}

export class UpdateServerSuccess implements Action {
  readonly type = ServerActionTypes.UPDATE_SUCCESS;

  constructor(public payload: ServerSuccessPayload) { }
}

export class UpdateServerFailure implements Action {
  readonly type = ServerActionTypes.UPDATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export interface UsersSuccessPayload {
  users: User[];
}

export class GetUsers implements Action {
  readonly type = ServerActionTypes.GET_USERS;
  constructor(public payload: {server_id: string} ) { }
}

export class GetUsersSuccess implements Action {
  readonly type = ServerActionTypes.GET_USERS_SUCCESS;

  constructor(public payload: UsersSuccessPayload) { }
}

export class GetUsersFailure implements Action {
  readonly type = ServerActionTypes.GET_USERS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateWebUIKey implements Action {
  readonly type = ServerActionTypes.UPDATE_WEB_UI_KEY;

  constructor(public payload: WebUIKey) { }
}

export class UpdateWebUIKeySuccess implements Action {
  readonly type = ServerActionTypes.UPDATE_WEB_UI_KEY_SUCCESS;

  constructor(public payload: WebUIKey) { }
}

export class UpdateWebUIKeyFailure implements Action {
  readonly type = ServerActionTypes.UPDATE_WEB_UI_KEY_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class ValidateWebUIKey implements Action {
  readonly type = ServerActionTypes.VALIDATE_WEB_UI_KEY;

  constructor(public payload: Server) { }
}

export class ValidateWebUIKeySuccess implements Action {
  readonly type = ServerActionTypes.VALIDATE_WEB_UI_KEY_SUCCESS;

  constructor(public payload: ValidateWebUIKeyResponse) { }
}

export class ValidateWebUIKeySuccessNot implements Action {
  readonly type = ServerActionTypes.VALIDATE_WEB_UI_KEY_SUCCESS_NOT;

  constructor(public payload: ValidateWebUIKeyResponse) { }
}

export class ValidateWebUIKeyFailure implements Action {
  readonly type = ServerActionTypes.VALIDATE_WEB_UI_KEY_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class GetMigrationStatus implements Action {
  readonly type = ServerActionTypes.GET_MIGRATION_STATUS;

  constructor(public payload: string) { }
}

export class GetMigrationStatusSuccess implements Action {
  readonly type = ServerActionTypes.GET_MIGRATION_STATUS_SUCCESS;

  constructor(public payload: MigrationStatus) { }
}

export class GetMigrationStatusFailure implements Action {
  readonly type = ServerActionTypes.GET_MIGRATION_STATUS_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type ServerActions =
  | GetServers
  | GetServersSuccess
  | GetServersFailure
  | GetServer
  | GetServerSuccess
  | GetServerFailure
  | CreateServer
  | CreateServerSuccess
  | CreateServerFailure
  | UpdateServer
  | UpdateServerSuccess
  | UpdateServerFailure
  | DeleteServer
  | DeleteServerSuccess
  | DeleteServerFailure
  | GetUsers
  | GetUsersSuccess
  | GetUsersFailure
  | UpdateWebUIKey
  | UpdateWebUIKeySuccess
  | UpdateWebUIKeyFailure
  | ValidateWebUIKey
  | ValidateWebUIKeySuccess
  | ValidateWebUIKeySuccessNot
  | ValidateWebUIKeyFailure
  | GetMigrationStatus
  | GetMigrationStatusSuccess
  | GetMigrationStatusFailure;

