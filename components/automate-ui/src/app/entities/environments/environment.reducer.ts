import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe, unset } from 'lodash/fp';
import { HttpErrorResponse } from '@angular/common/http';
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
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';
const DELETE_STATUS = 'deleteStatus';

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
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess),
        set('environmentList.items', action.payload.environments || []),
        set('environmentList.total', action.payload.total || 0)
        )(state) as EnvironmentEntityState;

    case EnvironmentActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);


    case EnvironmentActionTypes.CREATE: {
      return set(
        SAVE_STATUS,
        EntityStatus.loading,
        state);
    }

    case EnvironmentActionTypes.CREATE_SUCCESS: {
      return pipe(
        unset(SAVE_ERROR),
        set(SAVE_STATUS, EntityStatus.loadingSuccess)
      )(environmentEntityAdapter.addOne(action.payload, state)
      ) as EnvironmentEntityState;
    }

    case EnvironmentActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as EnvironmentEntityState;
    }

    case EnvironmentActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case EnvironmentActionTypes.DELETE_SUCCESS:
      const environments =
        state.environmentList.items.filter(environment => environment.name !== action.payload.name);
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
