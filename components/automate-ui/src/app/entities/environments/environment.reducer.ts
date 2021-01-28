import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { EnvironmentActionTypes, EnvironmentActions } from './environment.action';
import { Environment } from './environment.model';

export interface EnvironmentEntityState extends EntityState<Environment> {
  environmentsStatus: EntityStatus;
  getAllStatus: EntityStatus;
  environmentList: {
    items: Environment[],
    total: number
  };
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const DELETE_STATUS = 'deleteStatus'
export const environmentEntityAdapter: EntityAdapter<Environment> =
  createEntityAdapter<Environment>({
  selectId: (environment: Environment) => environment.name
});

export const EnvironmentEntityInitialState: EnvironmentEntityState =
  environmentEntityAdapter.getInitialState(<EnvironmentEntityState>{
  getAllStatus: EntityStatus.notLoaded,
  deleteStatus: EntityStatus.notLoaded
});

export function environmentEntityReducer(
  state: EnvironmentEntityState = EnvironmentEntityInitialState,
  action: EnvironmentActions): EnvironmentEntityState {

  switch (action.type) {
    case EnvironmentActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, environmentEntityAdapter
        .removeAll(state));

    case EnvironmentActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set('environmentList.items', action.payload.environments || []),
        set('environmentList.total', action.payload.total || 0)
        )(state) as EnvironmentEntityState;

    case EnvironmentActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case EnvironmentActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);
      
    case EnvironmentActionTypes.DELETE_SUCCESS:
      const environments = state.environmentList.items.filter(environment => environment.name !== action.payload.name );
      const total = state.environmentList.total - 1;
      return pipe(
        set(DELETE_STATUS, EntityStatus.loadingSuccess),
        set('environmentList.items', environments || []),
        set('environmentList.total', total || 0 )
      )(state) as EnvironmentEntityState;

    case EnvironmentActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);
    
    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: EnvironmentEntityState) => state.entities[id];
