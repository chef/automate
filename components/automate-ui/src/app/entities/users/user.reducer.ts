import { HttpErrorResponse } from '@angular/common/http';
import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { UserActionTypes, UserActions } from './user.actions';
import { User } from './user.model';

export interface UserEntityState extends EntityState<User> {
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
  createStatus: EntityStatus;
  createError: HttpErrorResponse;
}

export const userEntityAdapter: EntityAdapter<User> = createEntityAdapter<User>();

export const UserEntityInitialState: UserEntityState = userEntityAdapter.getInitialState({
  getStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded,
  createStatus: EntityStatus.notLoaded,
  createError: null
});

export function userEntityReducer(state: UserEntityState = UserEntityInitialState,
  action: UserActions): UserEntityState {

  switch (action.type) {

    case UserActionTypes.GET_ALL:
      return set('getStatus', EntityStatus.loading, state);

    case UserActionTypes.GET_ALL_SUCCESS:
      return set('getStatus', EntityStatus.loadingSuccess,
        userEntityAdapter.setAll(action.payload.users, state));

    case UserActionTypes.GET_ALL_FAILURE:
      return set('getStatus', EntityStatus.loadingFailure, state);

    case UserActionTypes.GET:
      return set(
        'getStatus',
        EntityStatus.loading,
        // clear user state to ensure we fetch the latest data
        userEntityAdapter.removeAll(state)
      ) as UserEntityState;

    case UserActionTypes.GET_SUCCESS:
      return set('getStatus', EntityStatus.loadingSuccess,
        userEntityAdapter.addOne(action.payload, state));

    case UserActionTypes.GET_FAILURE:
      return set('getStatus', EntityStatus.loadingFailure, state);

    case UserActionTypes.UPDATE_PASSWORD_USER:
      return set('updateStatus', EntityStatus.loading, state);

    case UserActionTypes.UPDATE_PASSWORD_USER_SUCCESS:
      return set('updateStatus', EntityStatus.loadingSuccess,
        userEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload}, state));

    case UserActionTypes.UPDATE_USER_FAILURE:
      return set('updateStatus', EntityStatus.loadingFailure, state);

    case UserActionTypes.UPDATE_NAME_USER:
      return set('updateStatus', EntityStatus.loading, state);

    case UserActionTypes.UPDATE_NAME_USER_SUCCESS:
      return set('updateStatus', EntityStatus.loadingSuccess,
        userEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload}, state));

    case UserActionTypes.DELETE:
      return set('deleteStatus', EntityStatus.loading, state);

    case UserActionTypes.DELETE_SUCCESS:
      return set('deleteStatus', EntityStatus.loadingSuccess,
        userEntityAdapter.removeOne(action.payload.id, state));

    case UserActionTypes.DELETE_FAILURE:
      return set('deleteStatus', EntityStatus.loadingFailure, state);

    case UserActionTypes.CREATE:
      return pipe(
        set('createError', null),
        set('createStatus', EntityStatus.loading)
      )(state) as UserEntityState;

    case UserActionTypes.CREATE_SUCCESS:
      return pipe(
        set('createError', null),
        set('createStatus', EntityStatus.loadingSuccess)
      )(userEntityAdapter.addOne(action.payload, state)) as UserEntityState;

    case UserActionTypes.CREATE_FAILURE:
      return pipe(
        set('createError', action.payload),
        set('createStatus', EntityStatus.loadingFailure)
      )(state) as UserEntityState;

    default:
      return state;

  }
}
