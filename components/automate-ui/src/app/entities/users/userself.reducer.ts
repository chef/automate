import { set, pipe } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { UserSelfActionTypes, UserSelfActions } from './userself.actions';
import { SelfUser } from './userself.model';

export interface UserSelfEntityState {
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
  userSelfId: string;
  userSelf: SelfUser;
}

export const UserSelfEntityInitialState: UserSelfEntityState = {
  getStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded,
  userSelfId: null,
  userSelf: null
};

export function userSelfEntityReducer(state: UserSelfEntityState = UserSelfEntityInitialState,
  action: UserSelfActions): UserSelfEntityState {

  switch (action.type) {
    case UserSelfActionTypes.SET_ID:
      return set('userSelfId', action.payload.id, state) as UserSelfEntityState;

    case UserSelfActionTypes.GET:
      return pipe(
        set('getStatus', EntityStatus.loading),
        set('userSelf', null)
      )(state) as UserSelfEntityState;

    case UserSelfActionTypes.GET_SUCCESS:
      return pipe(
        set('getStatus', EntityStatus.loadingSuccess),
        set('userSelf', action.payload)
      )(state) as UserSelfEntityState;

    case UserSelfActionTypes.GET_FAILURE:
      return set('getStatus', EntityStatus.loadingFailure, state);

    case UserSelfActionTypes.UPDATE_PASSWORD_SELF:
      return set('updateStatus', EntityStatus.loading, state);

    case UserSelfActionTypes.UPDATE_PASSWORD_SELF_SUCCESS:
      return pipe(
        set('updateStatus', EntityStatus.loadingSuccess),
        set('userSelf', action.payload)
      )(state) as UserSelfEntityState;

    case UserSelfActionTypes.UPDATE_PASSWORD_SELF_FAILURE:
      return set('updateStatus', EntityStatus.loadingFailure, state);

    case UserSelfActionTypes.UPDATE_NAME_SELF:
      return set('updateStatus', EntityStatus.loading, state);

    case UserSelfActionTypes.UPDATE_NAME_SELF_SUCCESS:
      return pipe(
        set('updateStatus', EntityStatus.loadingSuccess),
        set('userSelf', action.payload)
      )(state) as UserSelfEntityState;

    case UserSelfActionTypes.UPDATE_NAME_SELF_FAILURE:
      return set('updateStatus', EntityStatus.loadingFailure, state);

    default:
      return state;

  }
}
