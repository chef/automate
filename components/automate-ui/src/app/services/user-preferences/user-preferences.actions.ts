import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import {
  UserPreferencesPayload,
  UserPreferenceResponse,
  UserPreferenceTimeformat
} from './user-preferences.model';

export enum UserPreferencesActionTypes {
  GET            = 'USER_PREFERENCES::GET',
  GET_SUCCESS    = 'USER_PREFERENCES::GET::SUCCESS',
  GET_FAILURE    = 'USER_PREFERENCES::GET::FAILURE',
  UPDATE         = 'USER_PREFERENCES::UPDATE',
  UPDATE_SUCCESS = 'USER_PREFERENCES::UPDATE::SUCCESS',
  UPDATE_FAILURE = 'USER_PREFERENCES::UPDATE::FAILURE',
  SET_TIMEFORMAT = 'USER_PREFERENCES::SET::TIMEFORMAT'
}

export class GetUserPreferences implements Action {
  readonly type = UserPreferencesActionTypes.GET;
}

export class GetUserPreferencesSuccess implements Action {
  readonly type = UserPreferencesActionTypes.GET_SUCCESS;

  constructor(public payload: UserPreferenceResponse) {}
}

export class GetUserPreferencesFailure implements Action {
  readonly type = UserPreferencesActionTypes.GET_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateUserPreferences implements Action {
  readonly type = UserPreferencesActionTypes.UPDATE;
  constructor(public payload: UserPreferencesPayload) {}
}

export class UpdateUserPreferencesSuccess implements Action {
  readonly type = UserPreferencesActionTypes.UPDATE_SUCCESS;
  constructor(public payload: any) {}
}

export class UpdateUserPreferencesFailure implements Action {
  readonly type = UserPreferencesActionTypes.UPDATE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class SetUserTimeformatInternal implements Action {
  readonly type = UserPreferencesActionTypes.SET_TIMEFORMAT;

  constructor(public payload: UserPreferenceTimeformat) {}
}

export type UserPreferencesActions =
  | GetUserPreferences
  | GetUserPreferencesSuccess
  | GetUserPreferencesFailure
  | UpdateUserPreferences
  | UpdateUserPreferencesSuccess
  | UpdateUserPreferencesFailure
  | SetUserTimeformatInternal;
