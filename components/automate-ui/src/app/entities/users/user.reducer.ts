import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { UserActionTypes, UserActions } from './user.actions';
import { User } from './user.model';

export interface UserEntityState extends EntityState<User> {
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
  createStatus: EntityStatus;
}

export const userEntityAdapter: EntityAdapter<User> = createEntityAdapter<User>();

export const UserEntityInitialState: UserEntityState = userEntityAdapter.getInitialState({
  getStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded,
  createStatus: EntityStatus.notLoaded
});

export function userEntityReducer(state: UserEntityState = UserEntityInitialState,
  action: UserActions) {

  switch (action.type) {

    case UserActionTypes.GET_ALL:
      return set(
        'getStatus',
        EntityStatus.loading,
        // clear user state to ensure we fetch the latest data
        userEntityAdapter.removeAll(state)
      ) as UserEntityState;

    case UserActionTypes.GET_ALL_SUCCESS:
      return set('getStatus', EntityStatus.loadingSuccess,
        userEntityAdapter.addAll(action.payload.users, state));

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

    case UserActionTypes.UPDATE:
      return set('updateStatus', EntityStatus.loading, state);

    case UserActionTypes.UPDATE_SUCCESS:
      return set('updateStatus', EntityStatus.loadingSuccess,
        userEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload}, state));

    case UserActionTypes.UPDATE_FAILURE:
      return set('updateStatus', EntityStatus.loadingFailure, state);

    case UserActionTypes.UPDATE_SELF:
      return set('updateStatus', EntityStatus.loading, state);

    case UserActionTypes.UPDATE_SELF_SUCCESS:
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
      return set('createStatus', EntityStatus.loading, state);

    case UserActionTypes.CREATE_SUCCESS:
      return set('createStatus', EntityStatus.loadingSuccess,
                  userEntityAdapter.addOne(action.payload, state));

    case UserActionTypes.CREATE_FAILURE:
      return set('createStatus', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
