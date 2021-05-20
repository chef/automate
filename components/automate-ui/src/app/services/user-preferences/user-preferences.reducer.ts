import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { UserPreferencesActionTypes, UserPreferencesActions } from './user-preferences.actions';
import { UserPreference } from './user-preferences.model';

export interface UserPreferencesEntityState {
  list: {
    timeformat: UserPreference
  };
  error: HttpErrorResponse;
  status: EntityStatus;
}

export const UserPreferencesEntityInitialState: UserPreferencesEntityState = {
  list: {
    // Subject to change, enabled for development purposes
    timeformat: {
      // value 'DD MM YY'
      value: 'ddd, DD MMM YYYY',
      disabled: false
    }
  },
  error: null,
  status: EntityStatus.notLoaded
};

export function userPreferencesEntityReducer(
  action: UserPreferencesActions, 
  state: UserPreferencesEntityState = UserPreferencesEntityInitialState): UserPreferencesEntityState {

  switch (action.type) {

    case UserPreferencesActionTypes.GET_USER_PREFERENCES:
      return set('status', EntityStatus.loading, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_USER_PREFERENCES_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('list', action.payload.user_preferences))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_USER_PREFERENCES_FAILURE:
    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_FAILURE: // fallthrough
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('error', action.payload))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES:
      return set('status', EntityStatus.loading, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('list', action.payload))(state) as UserPreferencesEntityState;

    // This is purely hacky way to change timeformat for dev purposes only
    case UserPreferencesActionTypes.TEST_UPDATE_USER_TIMEFORMAT:
      console.log('not updating yet - user-prefs.reducer.ts');
      console.log(action.payload);

      const timeformat = {
        'value': action.payload,
        'disabled': false
      };

      return set('list.timeformat', timeformat, state);

    default:
      return state;
  }
}
