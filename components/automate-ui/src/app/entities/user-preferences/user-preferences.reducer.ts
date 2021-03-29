import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from '../entities';
import { UserPreferencesActionTypes, UserPreferencesActions } from './user-preferences.actions';

import {
  UserPreference
} from './user-preferences.model';

export interface UserPreferencesEntityState {
  list: UserPreference[];
  error: HttpErrorResponse;
  status: EntityStatus;
}

export const UserPreferencesEntityInitialState: UserPreferencesEntityState = {
  list: [],
  error: null,
  status: EntityStatus.notLoaded
};

export function serviceGroupsEntityReducer(
  state: UserPreferencesEntityState = UserPreferencesEntityInitialState,
  action: UserPreferencesActions): UserPreferencesEntityState {

  switch (action.type) {

    case UserPreferencesActionTypes.GET_USER_PREFERENCES:
      return pipe(
        set('status', EntityStatus.loading),
        set('list', {}))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_USER_PREFERENCES_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('list', action.payload.user_preferences))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_USER_PREFERENCES_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('error', action.payload))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES:
      return pipe(
        set('status', EntityStatus.loading),
        set('list', {}))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('list', action.payload))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_FAILURE:
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('error', action.payload))(state) as UserPreferencesEntityState;

    default:
      return state;
  }
}
