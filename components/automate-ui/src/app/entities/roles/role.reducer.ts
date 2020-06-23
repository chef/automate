import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { RoleActionTypes, RoleActions } from './role.actions';
import { Role } from './role.model';

export interface RoleEntityState extends EntityState<Role> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  deleteStatus: EntityStatus;
}

export const roleEntityAdapter: EntityAdapter<Role> = createEntityAdapter<Role>();

export const RoleEntityInitialState: RoleEntityState = roleEntityAdapter.getInitialState({
  getAllStatus: EntityStatus.notLoaded,
  getStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded
});

export function roleEntityReducer(state: RoleEntityState = RoleEntityInitialState,
  action: RoleActions): RoleEntityState {

  switch (action.type) {
    case RoleActionTypes.GET_ALL:
      return set('getAllStatus', EntityStatus.loading, state);

    case RoleActionTypes.GET_ALL_SUCCESS:
      return set('getAllStatus', EntityStatus.loadingSuccess,
        roleEntityAdapter.setAll(action.payload.roles, state));

    case RoleActionTypes.GET_ALL_FAILURE:
      return set('getAllStatus', EntityStatus.loadingFailure, state);

    case RoleActionTypes.GET:
      return set('getStatus', EntityStatus.loading, roleEntityAdapter.removeAll(state));

    case RoleActionTypes.GET_SUCCESS:
      return set('getStatus', EntityStatus.loadingSuccess,
        roleEntityAdapter.addOne(action.payload, state));

    case RoleActionTypes.GET_FAILURE:
      return set('getStatus', EntityStatus.loadingFailure, state);

    case RoleActionTypes.DELETE:
      return set('deleteStatus', EntityStatus.loading, state);

    case RoleActionTypes.DELETE_SUCCESS:
      return set('deleteStatus', EntityStatus.loadingSuccess,
        roleEntityAdapter.removeOne(action.payload.id, state));

    case RoleActionTypes.DELETE_FAILURE:
      return set('deleteStatus', EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
