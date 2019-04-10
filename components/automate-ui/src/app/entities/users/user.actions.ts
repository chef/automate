import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { User } from './user.model';
import { SelfUser } from './userself.model';

export enum UserActionTypes {
  DELETE                  = 'USER::DELETE',
  DELETE_SUCCESS          = 'USER::DELETE::SUCCESS',
  DELETE_FAILURE          = 'USER::DELETE::FAILURE',
  GET_ALL                 = 'USER::GET_ALL',
  GET_ALL_SUCCESS         = 'USER::GET_ALL::SUCCESS',
  GET_ALL_FAILURE         = 'USER::GET_ALL::FAILURE',
  GET_BY_USERNAME         = 'USER::GET_BY_USERNAME',
  GET_BY_USERNAME_SUCCESS = 'USER::GET_BY_USERNAME::SUCCESS',
  GET_BY_USERNAME_FAILURE = 'USER::GET_BY_USERNAME::FAILURE',
  UPDATE                  = 'USER::UPDATE',
  UPDATE_SUCCESS          = 'USER::UPDATE::SUCCESS',
  UPDATE_FAILURE          = 'USER::UPDATE::FAILURE',
  UPDATE_SELF             = 'USER::SELF::UPDATE',
  UPDATE_SELF_SUCCESS     = 'USER::SELF::UPDATE::SUCCESS',
  CREATE                  = 'USER::CREATE',
  CREATE_SUCCESS          = 'USER::CREATE::SUCCESS',
  CREATE_FAILURE          = 'USER::CREATE::FAILURE'
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

export class GetUserByUsername implements Action {
  readonly type = UserActionTypes.GET_BY_USERNAME;
  constructor(public payload: {username: string}) { }
}

export class GetUserByUsernameSuccess implements Action {
  readonly type = UserActionTypes.GET_BY_USERNAME_SUCCESS;
  constructor(public payload: User) { }
}

export class GetUserByUsernameFailure implements Action {
  readonly type = UserActionTypes.GET_BY_USERNAME_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateUser implements Action {
  readonly type = UserActionTypes.UPDATE;
  constructor(public payload: User) { }
}

export class UpdateUserSuccess implements Action {
  readonly type = UserActionTypes.UPDATE_SUCCESS;
  constructor(public payload: User) { }
}

export class UpdateUserFailure implements Action {
  readonly type = UserActionTypes.UPDATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateSelf implements Action {
  readonly type = UserActionTypes.UPDATE_SELF;
  constructor(public payload: SelfUser) { }
}

export class UpdateSelfSuccess implements Action {
  readonly type = UserActionTypes.UPDATE_SELF_SUCCESS;
  constructor(public payload: SelfUser) { }
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
  name: string;
  username: string;
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
  | GetUserByUsername
  | GetUserByUsernameSuccess
  | GetUserByUsernameFailure
  | UpdateUser
  | UpdateUserSuccess
  | UpdateUserFailure
  | UpdateSelf
  | UpdateSelfSuccess
  | CreateUser
  | CreateUserSuccess
  | CreateUserFailure;
