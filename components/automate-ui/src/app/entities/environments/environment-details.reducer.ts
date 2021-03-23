import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { EnvironmentActionTypes, EnvironmentActions } from './environment.action';
import { Environment } from './environment.model';

export interface EnvironmentDetailsEntityState extends EntityState<Environment> {
  environmentStatus: EntityStatus;
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
}

const GET_STATUS = 'getStatus';
const UPDATE_STATUS = 'updateStatus';

export const environmentDetailsEntityAdapter: EntityAdapter<Environment> =
createEntityAdapter<Environment>({
  selectId: (environment: Environment) => environment.name
});

export const EnvironmentEntityInitialState: EnvironmentDetailsEntityState =
  environmentDetailsEntityAdapter.getInitialState(<EnvironmentDetailsEntityState>{
  getStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded
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

    case EnvironmentActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state) as EnvironmentDetailsEntityState;

    case EnvironmentActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        environmentDetailsEntityAdapter.updateOne({
          id: action.payload.name,
          changes: action.payload
        }, state)) as EnvironmentDetailsEntityState;

    case EnvironmentActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS,
        EntityStatus.loadingFailure, state) as EnvironmentDetailsEntityState;


    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: EnvironmentDetailsEntityState) => state.entities[id];
