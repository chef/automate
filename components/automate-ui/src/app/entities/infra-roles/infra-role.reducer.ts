import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { RoleActionTypes, RoleActions } from './infra-role.action';
import { InfraRole, RoleItem } from './infra-role.model';

export interface InfraRoleEntityState extends EntityState<InfraRole> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
}

export interface RoleItemEntityState extends EntityState<RoleItem> {
    getAllStatus: EntityStatus;
    getStatusItem: EntityStatus;
  }

const GET_ALL_STATUS = 'getAllStatus';
const GET_STATUS = 'getStatus';

export const infraRoleEntityAdapter: EntityAdapter<InfraRole> = createEntityAdapter<InfraRole>({
  selectId: selectUserId
});

export const roleItemEntityAdapter: EntityAdapter<RoleItem> = createEntityAdapter<RoleItem>({
    selectId: (role: RoleItem) => role.name
  });

export function selectUserId(a: InfraRole): string {
    //In this case this would be optional since primary key is id
    return a.name;
  }

export const InfraRoleEntityInitialState: InfraRoleEntityState =
infraRoleEntityAdapter.getInitialState(<InfraRoleEntityState>{
    getAllStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded
  });

export function infraRoleEntityReducer(
  state: InfraRoleEntityState = InfraRoleEntityInitialState,
  action: RoleActions): InfraRoleEntityState {

  switch (action.type) {
    case RoleActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, infraRoleEntityAdapter.removeAll(state));

    case RoleActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (infraRoleEntityAdapter.addAll(action.payload.roles, state)) as InfraRoleEntityState;

    case RoleActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);
    
    case RoleActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, infraRoleEntityAdapter.removeAll(state));
  
    case RoleActionTypes.GET_SUCCESS:
       return set(GET_STATUS, EntityStatus.loadingSuccess,
        infraRoleEntityAdapter.addOne(action.payload.name, state));
  
    case RoleActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (name: string) => (state: InfraRoleEntityState) => state.entities[name];
