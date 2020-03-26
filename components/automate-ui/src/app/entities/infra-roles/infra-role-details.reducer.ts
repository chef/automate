import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { RoleActionTypes, RoleActions } from './infra-role.action';
import { InfraRole } from './infra-role.model';

export interface InfraRoleDetailsEntityState extends EntityState<InfraRole> {
  status: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const infraRoleDetailsEntityAdapter: EntityAdapter<InfraRole> =
createEntityAdapter<InfraRole>({
  selectId: (infrarole: InfraRole) => infrarole.name
});

export const InfraRoleEntityInitialState: InfraRoleDetailsEntityState =
infraRoleDetailsEntityAdapter.getInitialState(<InfraRoleDetailsEntityState>{
  getStatus: EntityStatus.notLoaded
});

export function infraRoleDetailsEntityReducer(
  state: InfraRoleDetailsEntityState = InfraRoleEntityInitialState,
  action: RoleActions): InfraRoleDetailsEntityState {

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

    default:
      return state;
  }
}
