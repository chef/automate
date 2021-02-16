import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { EnvironmentActionTypes, EnvironmentActions } from './environment.action';
import { Environment } from './environment.model';

export interface EnvironmentEntityState extends EntityState<Environment> {
  environmentsStatus: EntityStatus;
  getAllStatus: EntityStatus;
  getSearchStatus: EntityStatus;
  environmentList: {
    items: Environment[],
    total: number
  };
}

const GET_SEARCH_STATUS = 'getSearchStatus';

export const environmentEntityAdapter: EntityAdapter<Environment> =
  createEntityAdapter<Environment>({
  selectId: (environment: Environment) => environment.name
});

export const EnvironmentEntityInitialState: EnvironmentEntityState =
  environmentEntityAdapter.getInitialState(<EnvironmentEntityState>{
  getAllStatus: EntityStatus.notLoaded
});

export function environmentEntityReducer(
  state: EnvironmentEntityState = EnvironmentEntityInitialState,
  action: EnvironmentActions): EnvironmentEntityState {

  switch (action.type) {
    case EnvironmentActionTypes.SEARCH:
      return set(GET_SEARCH_STATUS, EntityStatus.loading, environmentEntityAdapter
        .removeAll(state));

    case EnvironmentActionTypes.SEARCH_SUCCESS:
      return pipe(
        set('environmentList.items', action.payload.environments || []),
        set('environmentList.total', action.payload.total || 0)
        )(state) as EnvironmentEntityState;

    case EnvironmentActionTypes.SEARCH_FAILURE:
      return set(GET_SEARCH_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}

export const getEntityById = (id: string) =>
  (state: EnvironmentEntityState) => state.entities[id];
