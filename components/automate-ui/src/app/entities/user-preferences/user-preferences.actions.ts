import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import {
  UserPreference,
  UserPreferencesPayload
} from './user-preferences.model';

export enum UserPreferencesActionTypes {
  GET_USER_PREFERENCES         = 'USER_PREFERENCES::GET',
  GET_USER_PREFERENCES_SUCCESS = 'USER_PREFERENCES::GET::SUCCESS',
  GET_USER_PREFERENCES_FAILURE = 'USER_PREFERENCES::GET::FAILURE',

  UPDATE_USER_PREFERENCES         = 'USER_PREFERENCES::UPDATE',
  UPDATE_USER_PREFERENCES_SUCCESS = 'USER_PREFERENCES::UPDATE::SUCCESS',
  UPDATE_USER_PREFERENCES_FAILURE = 'USER_PREFERENCES::UPDATE::FAILURE'
}

export class GetUserPreferences implements Action {
  readonly type = UserPreferencesActionTypes.GET_USER_PREFERENCES;
  constructor() {}
}

export class GetUserPreferencesSuccess implements Action {
  readonly type = UserPreferencesActionTypes.GET_USER_PREFERENCES_SUCCESS;

  constructor(public payload: UserPreferencesPayload) {}
}

export class GetUserPreferencesFailure implements Action {
  readonly type = UserPreferencesActionTypes.GET_USER_PREFERENCES_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateUserPreferences implements Action {
  readonly type = UserPreferencesActionTypes.UPDATE_USER_PREFERENCES;
  constructor(public payload: UserPreference[]) {}
}

export class UpdateUserPreferencesSuccess implements Action {
  readonly type = UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_SUCCESS;

  constructor(public payload: UserPreferencesPayload) {}
}

export class UpdateUserPreferencesFailure implements Action {
  readonly type = UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type UserPreferencesActions =
  | GetUserPreferences
  | GetUserPreferencesSuccess
  | GetUserPreferencesFailure
  | UpdateUserPreferences
  | UpdateUserPreferencesSuccess
  | UpdateUserPreferencesFailure;
