import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { ApiToken } from './api-token.model';

export enum ApiTokenActionTypes {
  GET_ALL              = 'TOKEN::GET_ALL',
  GET_ALL_SUCCESS      = 'TOKEN::GET_ALL::SUCCESS',
  GET_ALL_FAILURE      = 'TOKEN::GET_ALL::FAILURE',
  GET                  = 'TOKEN::GET',
  GET_SUCCESS          = 'TOKEN::GET::SUCCESS',
  GET_FAILURE          = 'TOKEN::GET::FAILURE',
  UPDATE               = 'TOKEN::UPDATE',
  UPDATE_SUCCESS       = 'TOKEN::UPDATE::SUCCESS',
  UPDATE_FAILURE       = 'TOKEN::UPDATE::FAILURE',
  CREATE               = 'TOKEN::CREATE',
  CREATE_SUCCESS       = 'TOKEN::CREATE::SUCCESS',
  CREATE_FAILURE       = 'TOKEN::CREATE::FAILURE',
  DELETE               = 'TOKEN::DELETE',
  DELETE_SUCCESS       = 'TOKEN::DELETE::SUCCESS',
  DELETE_FAILURE       = 'TOKEN::DELETE::FAILURE',
  TOGGLE               = 'TOKEN::TOGGLE',
  TOGGLE_SUCCESS       = 'TOKEN::TOGGLE::SUCCESS',
  TOGGLE_FAILURE       = 'TOKEN::TOGGLE::FAILURE'
}

export class GetAllTokens implements Action {
  readonly type = ApiTokenActionTypes.GET_ALL;
}

export interface GetAllTokensSuccessPayload {
  tokens: ApiToken[];
}

export class GetAllTokensSuccess implements Action {
  readonly type = ApiTokenActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: GetAllTokensSuccessPayload) { }
}

export class GetAllTokensFailure implements Action {
  readonly type = ApiTokenActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetToken implements Action {
  readonly type = ApiTokenActionTypes.GET;
  constructor(public payload: {id: string}) { }
}

export class GetTokenSuccess implements Action {
  readonly type = ApiTokenActionTypes.GET_SUCCESS;
  constructor(public payload: ApiToken) { }
}

export class GetTokenFailure implements Action {
  readonly type = ApiTokenActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateToken implements Action {
  readonly type = ApiTokenActionTypes.UPDATE;
  constructor(public payload: {token: ApiToken}) { }
}

export class UpdateTokenSuccess implements Action {
  readonly type = ApiTokenActionTypes.UPDATE_SUCCESS;
  constructor(public payload: ApiToken) { }
}

export class UpdateTokenFailure implements Action {
  readonly type = ApiTokenActionTypes.UPDATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class CreateToken implements Action {
  readonly type = ApiTokenActionTypes.CREATE;
  constructor(public payload: {id: string, name: string}) { }
}

export class CreateTokenSuccess implements Action {
  readonly type = ApiTokenActionTypes.CREATE_SUCCESS;
  constructor(public payload: ApiToken) { }
}

export class CreateTokenFailure implements Action {
  readonly type = ApiTokenActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteToken implements Action {
  readonly type = ApiTokenActionTypes.DELETE;
  constructor(public payload: ApiToken) { }
}

export class DeleteTokenSuccess implements Action {
  readonly type = ApiTokenActionTypes.DELETE_SUCCESS;
  constructor(public payload: ApiToken) { }
}

export class DeleteTokenFailure implements Action {
  readonly type = ApiTokenActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class ToggleTokenActive implements Action {
  readonly type = ApiTokenActionTypes.TOGGLE;
  constructor(public payload: ApiToken) { }
}

export class ToggleTokenActiveSuccess implements Action {
  readonly type = ApiTokenActionTypes.TOGGLE_SUCCESS;
  constructor(public payload: ApiToken) { }
}

export class ToggleTokenActiveFailure implements Action {
  readonly type = ApiTokenActionTypes.TOGGLE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type ApiTokenActions =
  | GetAllTokens
  | GetAllTokensSuccess
  | GetAllTokensFailure
  | GetToken
  | GetTokenSuccess
  | GetTokenFailure
  | UpdateToken
  | UpdateTokenSuccess
  | UpdateTokenFailure
  | CreateToken
  | CreateTokenSuccess
  | CreateTokenFailure
  | DeleteToken
  | DeleteTokenSuccess
  | DeleteTokenFailure
  | ToggleTokenActive
  | ToggleTokenActiveSuccess
  | ToggleTokenActiveFailure;
