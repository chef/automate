import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { User } from './user.model';

export enum UserActionTypes {
  DELETE                           = 'USER::DELETE',
  DELETE_SUCCESS                   = 'USER::DELETE::SUCCESS',
  DELETE_FAILURE                   = 'USER::DELETE::FAILURE',
  GET_ALL                          = 'USER::GET_ALL',
  GET_ALL_SUCCESS                  = 'USER::GET_ALL::SUCCESS',
  GET_ALL_FAILURE                  = 'USER::GET_ALL::FAILURE',
  GET                              = 'USER::GET',
  GET_SUCCESS                      = 'USER::GET::SUCCESS',
  GET_FAILURE                      = 'USER::GET::FAILURE',
  UPDATE_NAME_USER                 = 'USER::NAME::UPDATE',
  UPDATE_NAME_USER_SUCCESS         = 'USER::NAME::UPDATE::SUCCESS',
  UPDATE_PASSWORD_USER             = 'USER::PASSWORD::UPDATE',
  UPDATE_PASSWORD_USER_SUCCESS     = 'USER::PASSWORD::UPDATE::SUCCESS',
  UPDATE_USER_FAILURE              = 'USER::UPDATE::FAILURE',
  CREATE                           = 'USER::CREATE',
  CREATE_SUCCESS                   = 'USER::CREATE::SUCCESS',
  CREATE_FAILURE                   = 'USER::CREATE::FAILURE'
}

export class GetUsers implements Action {
  readonly type = UserActionTypes.GET_ALL;
}

export interface GetUsersSuccessPayload {
  users: User[];
}
export class GetUsersSuccess implements Action {
  readonly type = UserActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: GetUsersSuccessPayload) { }
}

export class GetUsersFailure implements Action {
  readonly type = UserActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetUser implements Action {
  readonly type = UserActionTypes.GET;
  constructor(public payload: {id: string}) { }
}

export class GetUserSuccess implements Action {
  readonly type = UserActionTypes.GET_SUCCESS;
  constructor(public payload: User) { }
}

export class GetUserFailure implements Action {
  readonly type = UserActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdatePasswordUser implements Action {
  readonly type = UserActionTypes.UPDATE_PASSWORD_USER;
  constructor(public payload: User) { }
}

export class UpdatePasswordUserSuccess implements Action {
  readonly type = UserActionTypes.UPDATE_PASSWORD_USER_SUCCESS;
  constructor(public payload: User) { }
}

export class UpdateUserFailure implements Action {
  readonly type = UserActionTypes.UPDATE_USER_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateNameUser implements Action {
  readonly type = UserActionTypes.UPDATE_NAME_USER;
  constructor(public payload: User) { }
}

export class UpdateNameUserSuccess implements Action {
  readonly type = UserActionTypes.UPDATE_NAME_USER_SUCCESS;
  constructor(public payload: User) { }
}

export class DeleteUser implements Action {
  readonly type = UserActionTypes.DELETE;
  constructor(public payload: User) { }
}

export class DeleteUserSuccess implements Action {
  readonly type = UserActionTypes.DELETE_SUCCESS;
  constructor(public payload: User) { }
}

export class DeleteUserFailure implements Action {
  readonly type = UserActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export interface CreateUserPayload {
  id: string;
  name: string;
  password: string;
}

export class CreateUser implements Action {
  readonly type = UserActionTypes.CREATE;

  constructor(public payload: CreateUserPayload) { }
}

export class CreateUserSuccess implements Action {
  readonly type = UserActionTypes.CREATE_SUCCESS;

  constructor(public payload: User) { }
}

export class CreateUserFailure implements Action {
  readonly type = UserActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type UserActions =
  | DeleteUser
  | DeleteUserSuccess
  | DeleteUserFailure
  | GetUsers
  | GetUsersSuccess
  | GetUsersFailure
  | GetUser
  | GetUserSuccess
  | GetUserFailure
  | UpdatePasswordUser
  | UpdatePasswordUserSuccess
  | UpdateNameUser
  | UpdateNameUserSuccess
  | UpdateUserFailure
  | CreateUser
  | CreateUserSuccess
  | CreateUserFailure;
