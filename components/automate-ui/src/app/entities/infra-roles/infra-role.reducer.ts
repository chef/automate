import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe, unset } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { RoleActionTypes, RoleActions } from './infra-role.action';
import { InfraRole } from './infra-role.model';

export interface InfraRoleEntityState extends EntityState<InfraRole> {
  rolesStatus:  EntityStatus;
  getAllStatus: EntityStatus;
  roleList: {
    items: InfraRole[],
    total: number
  };
  deleteStatus: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
}

const GET_ALL_STATUS = 'getAllStatus';
const DELETE_STATUS = 'deleteStatus';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';

export const infraRoleEntityAdapter: EntityAdapter<InfraRole> = createEntityAdapter<InfraRole>({
  selectId: (infraRole: InfraRole) => infraRole.name
});

export const InfraRoleEntityInitialState: InfraRoleEntityState =
  infraRoleEntityAdapter.getInitialState(<InfraRoleEntityState>{
  getAllStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded
});

export function infraRoleEntityReducer(
  state: InfraRoleEntityState = InfraRoleEntityInitialState,
  action: RoleActions): InfraRoleEntityState {

  switch (action.type) {
    case RoleActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, infraRoleEntityAdapter.removeAll(state));

    case RoleActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess),
        set('roleList.items', action.payload.roles || []),
        set('roleList.total', action.payload.total || 0)
        )(state) as InfraRoleEntityState;

    case RoleActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case RoleActionTypes.CREATE: {
      return set(
        SAVE_STATUS,
        EntityStatus.loading,
        state);
    }

    case RoleActionTypes.CREATE_SUCCESS: {
      return pipe(
        unset(SAVE_ERROR),
        set(SAVE_STATUS, EntityStatus.loadingSuccess)
      )(infraRoleEntityAdapter.addOne(action.payload, state)
      ) as InfraRoleEntityState;
    }

    case RoleActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as InfraRoleEntityState;
    }

    case RoleActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case RoleActionTypes.DELETE_SUCCESS:
      const roles = state.roleList.items.filter(role => role.name !== action.payload.name );
      const total = state.roleList.total - 1;
      return pipe(
        set(DELETE_STATUS, EntityStatus.loadingSuccess),
        set('roleList.items', roles || []),
        set('roleList.total', total || 0 )
        )(state) as InfraRoleEntityState;

    case RoleActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: InfraRoleEntityState) => state.entities[id];
