import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { UserActionTypes, UserActions } from './user.actions';
import { User } from './user.model';

export interface UserEntityState extends EntityState<User> {
  status: EntityStatus;
  updateStatus: EntityStatus;
}

export const userEntityAdapter: EntityAdapter<User> = createEntityAdapter<User>();

export const UserEntityInitialState: UserEntityState = userEntityAdapter.getInitialState({
  status: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded
});

export function userEntityReducer(state: UserEntityState = UserEntityInitialState,
  action: UserActions) {

  switch (action.type) {

    case UserActionTypes.GET_ALL:
      return set('status', EntityStatus.loading, state);

    case UserActionTypes.GET_ALL_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
        userEntityAdapter.addAll(action.payload.users, state));

    case UserActionTypes.GET_ALL_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case UserActionTypes.GET_BY_USERNAME:
      return set('status', EntityStatus.loading, state);

    case UserActionTypes.GET_BY_USERNAME_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
        userEntityAdapter.addOne(action.payload, state));

    case UserActionTypes.GET_BY_USERNAME_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

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
      return set('status', EntityStatus.loading, state);

    case UserActionTypes.DELETE_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
        userEntityAdapter.removeOne(action.payload.id, state));

    case UserActionTypes.DELETE_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    case UserActionTypes.CREATE:
      return set('status', EntityStatus.loading, state);

    case UserActionTypes.CREATE_SUCCESS:
      return set('status', EntityStatus.loadingSuccess,
                  userEntityAdapter.addOne(action.payload, state));

    case UserActionTypes.CREATE_FAILURE:
      return set('status', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
