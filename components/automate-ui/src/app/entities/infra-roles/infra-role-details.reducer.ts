import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from '../../entities/entities';
import { RoleActionTypes, RoleActions } from './infra-role.action';
import { InfraRole } from './infra-role.model';

export interface InfraRoleDetailsEntityState extends EntityState<InfraRole> {
  roleStatus: EntityStatus;
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';
const UPDATE_STATUS = 'updateStatus';

export const infraRoleDetailsEntityAdapter: EntityAdapter<InfraRole> =
createEntityAdapter<InfraRole>({
  selectId: (infraRole: InfraRole) => infraRole.name
});

export const InfraRoleEntityInitialState: InfraRoleDetailsEntityState =
  infraRoleDetailsEntityAdapter.getInitialState(<InfraRoleDetailsEntityState>{
    getStatus: EntityStatus.notLoaded,
    updateStatus: EntityStatus.notLoaded
  });

export function infraRoleDetailsEntityReducer(
  state: InfraRoleDetailsEntityState = InfraRoleEntityInitialState,
  action: RoleActions | any): InfraRoleDetailsEntityState {

  switch (action.type) {
    case RoleActionTypes.GET:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        infraRoleDetailsEntityAdapter.removeAll(state)
      ) as InfraRoleDetailsEntityState;

    case RoleActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        infraRoleDetailsEntityAdapter.addOne(action.payload, state));

    case RoleActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    case RoleActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state) as InfraRoleDetailsEntityState;

    case RoleActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        infraRoleDetailsEntityAdapter.updateOne({
          id: action.payload.name,
          changes: action.payload
        }, state)) as InfraRoleDetailsEntityState;

    case RoleActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS,
        EntityStatus.loadingFailure, state) as InfraRoleDetailsEntityState;

    default:
      return state;
  }
}
