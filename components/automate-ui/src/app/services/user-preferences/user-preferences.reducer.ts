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
        value: 'ddd, DD MMM YYYY HH:mm:ss [UTC]',
        valid_values: []
    }
  },
  error: null,
  status: EntityStatus.notLoaded
};

export function userPreferencesEntityReducer(
  state: UserPreferencesEntityState = UserPreferencesEntityInitialState,
  action: UserPreferencesActions): UserPreferencesEntityState {
  switch (action.type) {

    case UserPreferencesActionTypes.GET:
      return set('status', EntityStatus.loading, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_SUCCESS:
      return pipe(
        set('status', EntityStatus.loadingSuccess),
        set('list.timeformat', action.payload.time_format))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.GET_FAILURE:
    case UserPreferencesActionTypes.UPDATE_FAILURE: // fallthrough
      return pipe(
        set('status', EntityStatus.loadingFailure),
        set('error', action.payload))(state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE:
      return set('status', EntityStatus.loading, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.UPDATE_SUCCESS:
      return set('status', EntityStatus.loadingSuccess, state) as UserPreferencesEntityState;

    case UserPreferencesActionTypes.SET_TIMEFORMAT:
      const timeformat = { value: action.payload.value, valid_values: action.payload.valid_values };
      return set('list.timeformat', timeformat, state);

    default:
      return state;
  }
}
