import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { RoleEnvironmentActionTypes, RoleEnvironmentActions } from './role-environments.action';

export interface RoleEnvironmentEntityState extends EntityState<string> {
  roleEnvironmentsStatus: EntityStatus;
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const roleEnvironmentEntityAdapter: EntityAdapter<string> =
  createEntityAdapter<string>({
   selectId: (environments: string) => environments
});

export const RoleEnvironmentEntityInitialState: RoleEnvironmentEntityState =
  roleEnvironmentEntityAdapter.getInitialState(<RoleEnvironmentEntityState>{
  getAllStatus: EntityStatus.notLoaded
});

export function roleEnvironmentEntityReducer(
  state: RoleEnvironmentEntityState = RoleEnvironmentEntityInitialState,
  action: RoleEnvironmentActions): RoleEnvironmentEntityState {

  switch (action.type) {
    case RoleEnvironmentActionTypes.GET_ALL:
      return set(GET_ALL_STATUS,
        EntityStatus.loading, roleEnvironmentEntityAdapter.removeAll(state));

    case RoleEnvironmentActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (roleEnvironmentEntityAdapter.setAll(action.payload.environments, state)) as
        RoleEnvironmentEntityState;

    case RoleEnvironmentActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);


    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: RoleEnvironmentEntityState) => state.entities[id];
