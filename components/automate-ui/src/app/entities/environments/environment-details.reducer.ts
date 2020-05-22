import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { EnvironmentActionTypes, EnvironmentActions } from './environment.action';
import { Environment } from './environment.model';

export interface EnvironmentDetailsEntityState extends EntityState<Environment> {
  environmentStatus: EntityStatus;
  getStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';

export const environmentDetailsEntityAdapter: EntityAdapter<Environment> =
createEntityAdapter<Environment>({
  selectId: (environment: Environment) => environment.name
});

export const EnvironmentEntityInitialState: EnvironmentDetailsEntityState =
  environmentDetailsEntityAdapter.getInitialState(<EnvironmentDetailsEntityState>{
  getStatus: EntityStatus.notLoaded
});

export function environmentDetailsEntityReducer(
  state: EnvironmentDetailsEntityState = EnvironmentEntityInitialState,
  action: EnvironmentActions): EnvironmentDetailsEntityState {

  switch (action.type) {
    case EnvironmentActionTypes.GET:
      return set(
        GET_STATUS,
        EntityStatus.loading,
        environmentDetailsEntityAdapter.removeAll(state)
      ) as EnvironmentDetailsEntityState;

    case EnvironmentActionTypes.GET_SUCCESS:
       return set(GET_STATUS, EntityStatus.loadingSuccess,
        environmentDetailsEntityAdapter.addOne(action.payload, state));
    case EnvironmentActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
