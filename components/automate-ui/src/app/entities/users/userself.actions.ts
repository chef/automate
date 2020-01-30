import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { SelfUser } from './userself.model';

export enum UserSelfActionTypes {
  SET_ID                           = 'USERSELF::ID::SET',
  GET                              = 'USERSELF::GET',
  GET_SUCCESS                      = 'USERSELF::GET::SUCCESS',
  GET_FAILURE                      = 'USERSELF::GET::FAILURE',
  UPDATE_NAME_SELF                 = 'USERSELF::NAME::UPDATE',
  UPDATE_NAME_SELF_SUCCESS         = 'USERSELF::NAME::UPDATE::SUCCESS',
  UPDATE_NAME_SELF_FAILURE         = 'USERSELF::NAME::UPDATE::FAILURE',
  UPDATE_PASSWORD_SELF             = 'USERSELF::PASSWORD::UPDATE',
  UPDATE_PASSWORD_SELF_SUCCESS     = 'USERSELF::PASSWORD::UPDATE::SUCCESS',
  UPDATE_PASSWORD_SELF_FAILURE     = 'USERSELF::PASSWORD::UPDATE::FAILURE'
}

export class SetUserSelfID implements Action {
  readonly type = UserSelfActionTypes.SET_ID;
  constructor(public payload: {id: string}) { }
}

export class GetUserSelf implements Action {
  readonly type = UserSelfActionTypes.GET;
  constructor() { }
}

export class GetUserSelfSuccess implements Action {
  readonly type = UserSelfActionTypes.GET_SUCCESS;
  constructor(public payload: SelfUser) { }
}

export class GetUserSelfFailure implements Action {
  readonly type = UserSelfActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateNameSelf implements Action {
  readonly type = UserSelfActionTypes.UPDATE_NAME_SELF;
  constructor(public payload: SelfUser) { }
}

export class UpdateNameSelfSuccess implements Action {
  readonly type = UserSelfActionTypes.UPDATE_NAME_SELF_SUCCESS;
  constructor(public payload: SelfUser) { }
}

export class UpdateNameSelfFailure implements Action {
  readonly type = UserSelfActionTypes.UPDATE_NAME_SELF_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdatePasswordSelf implements Action {
  readonly type = UserSelfActionTypes.UPDATE_PASSWORD_SELF;
  constructor(public payload: SelfUser) { }
}

export class UpdatePasswordSelfSuccess implements Action {
  readonly type = UserSelfActionTypes.UPDATE_PASSWORD_SELF_SUCCESS;
  constructor(public payload: SelfUser) { }
}

export class UpdatePasswordSelfFailure implements Action {
  readonly type = UserSelfActionTypes.UPDATE_PASSWORD_SELF_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type UserSelfActions =
  | SetUserSelfID
  | GetUserSelf
  | GetUserSelfSuccess
  | GetUserSelfFailure
  | UpdatePasswordSelf
  | UpdatePasswordSelfSuccess
  | UpdatePasswordSelfFailure
  | UpdateNameSelf
  | UpdateNameSelfSuccess
  | UpdateNameSelfFailure;
