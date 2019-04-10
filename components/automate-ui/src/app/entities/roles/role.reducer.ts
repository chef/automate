import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { RoleActionTypes, RoleActions } from './role.actions';
import { Role } from './role.model';

export interface RoleEntityState extends EntityState<Role> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
}

export const roleEntityAdapter: EntityAdapter<Role> = createEntityAdapter<Role>();

export const RoleEntityInitialState: RoleEntityState = roleEntityAdapter.getInitialState({
  getAllStatus: EntityStatus.notLoaded,
  getStatus: EntityStatus.notLoaded
});

export function roleEntityReducer(state: RoleEntityState = RoleEntityInitialState,
  action: RoleActions) {

  switch (action.type) {
    case RoleActionTypes.GET_ALL:
      return set('getAllStatus', EntityStatus.loading, state);

    case RoleActionTypes.GET_ALL_SUCCESS:
      return set('getAllStatus', EntityStatus.loadingSuccess,
        roleEntityAdapter.addAll(action.payload.roles, state));

    case RoleActionTypes.GET_ALL_FAILURE:
      return set('getAllStatus', EntityStatus.loadingFailure, state);

    case RoleActionTypes.GET:
      return set('getStatus', EntityStatus.loading, state);

    case RoleActionTypes.GET_SUCCESS:
      return set('getStatus', EntityStatus.loadingSuccess,
        roleEntityAdapter.addOne(action.payload, state));

    case RoleActionTypes.GET_ALL_FAILURE:
      return set('getStatus', EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
