import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { UserPreferencesActionTypes, UserPreferencesActions } from './user-preferences.actions';
import { UserPreferenceTimeformat } from './user-preferences.model';

export interface UserPreferencesEntityState {
  list: {
    timeformat: UserPreferenceTimeformat
  };
  error: HttpErrorResponse;
  status: EntityStatus;
}

export const UserPreferencesEntityInitialState: UserPreferencesEntityState = {
  list: {
    timeformat: {
        value: 'ddd, DD MMM YYYY',
        valid_values: ['ddd, DD MMM YYYY HH:mm:ss [UTC]', 'YYYY-M-D',
          'ddd, DD MMM YYYY', 'DD MMM YYYY', 'ddd, DD MMM', 'YYYY-MM-DD']
    }
  },
  error: null,
  status: EntityStatus.notLoaded
};

export function userPreferencesEntityReducer(
  state: UserPreferencesEntityState = UserPreferencesEntityInitialState,
  action: UserPreferencesActions): UserPreferencesEntityState {
  switch (action.type) {

    case UserPreferencesActionTypes.GET_USER_PREFERENCES:
      return set('status', EntityStatus.loading, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_USER_PREFERENCES_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('list.timeformat', action.payload.time_format))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_USER_PREFERENCES_FAILURE:
    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_FAILURE: // fallthrough
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('error', action.payload))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES:
      return set('status', EntityStatus.loading, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_USER_PREFERENCES_SUCCESS:
      return set('status', EntityStatus.loadingSuccess, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.SAVE_TIMEFORMAT_INTERNAL:
      const timeformat = { value: action.payload };
      return set('list.timeformat', timeformat, state);

    default:
      return state;
  }
}
