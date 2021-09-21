import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { pipe, set, unset } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { DestinationActionTypes, DestinationActions } from './destination.actions';
import { Destination } from './destination.model';

export interface DestinationEntityState extends EntityState<Destination> {
  status: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
  enableStatus: EntityStatus;
  testConnectionStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';
const UPDATE_STATUS = 'updateStatus';
const GET_STATUS = 'getStatus';
const DELETE_STATUS = 'deleteStatus';
const ENABLE_STATUS = 'enableStatus';
const TEST_CONNECTION_STATUS = 'testConnectionStatus';

export const destinationEntityAdapter: EntityAdapter<Destination> =
  createEntityAdapter<Destination>();


export const DestinationEntityInitialState: DestinationEntityState =
  destinationEntityAdapter.getInitialState({
    status: EntityStatus.notLoaded,
    saveStatus: EntityStatus.notLoaded,
    saveError: null,
    updateStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded,
    deleteStatus: EntityStatus.notLoaded,
    enableStatus: EntityStatus.notLoaded,
    testConnectionStatus: EntityStatus.notLoaded
  });

export function destinationEntityReducer(
  state: DestinationEntityState = DestinationEntityInitialState,
  action: DestinationActions): DestinationEntityState {

  switch (action.type) {

    case DestinationActionTypes.GET_ALL: {
      return set(
        GET_ALL_STATUS,
        EntityStatus.loading,
        destinationEntityAdapter.removeAll(state)
      ) as DestinationEntityState;
    }

    case DestinationActionTypes.GET_ALL_SUCCESS: {
      return set(
        GET_ALL_STATUS,
        EntityStatus.loadingSuccess,
        destinationEntityAdapter.setAll(action.payload.destinations, state)
      ) as DestinationEntityState;
    }

    case DestinationActionTypes.GET_ALL_FAILURE: {
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state
        ) as DestinationEntityState;
    }

    case DestinationActionTypes.GET: {
      return set(
        GET_STATUS,
        EntityStatus.loading,
        destinationEntityAdapter.removeAll(state)
      ) as DestinationEntityState;
    }

    case DestinationActionTypes.GET_SUCCESS: {
      return set(
        GET_STATUS,
        EntityStatus.loadingSuccess,
        destinationEntityAdapter.addOne(action.payload, state)
      ) as DestinationEntityState;
    }

    case DestinationActionTypes.GET_FAILURE: {
      return set(GET_STATUS, EntityStatus.loadingFailure, state) as DestinationEntityState;
    }

    case DestinationActionTypes.CREATE: {
      return set(SAVE_STATUS, EntityStatus.loading, state) as DestinationEntityState;
    }

    case DestinationActionTypes.CREATE_SUCCESS: {
      return pipe(
        unset(SAVE_ERROR),
        set(SAVE_STATUS, EntityStatus.loadingSuccess)
      )(destinationEntityAdapter.addOne(action.payload, state)) as DestinationEntityState;
    }

    case DestinationActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as DestinationEntityState;
    }

    case DestinationActionTypes.DELETE: {
      return set(DELETE_STATUS, EntityStatus.loading, state);
    }

    case DestinationActionTypes.DELETE_SUCCESS: {
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        destinationEntityAdapter.removeOne(action.payload.id, state));
    }

    case DestinationActionTypes.DELETE_FAILURE: {
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);
    }

    case DestinationActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case DestinationActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        destinationEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload
        }, state));

    case DestinationActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

    case DestinationActionTypes.ENABLE_DISABLE:
      return set(ENABLE_STATUS, EntityStatus.loading, state);

    case DestinationActionTypes.ENABLE_DISABLE_SUCCESS:
      return set(ENABLE_STATUS, EntityStatus.loadingSuccess,
        destinationEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload
        }, state));

    case DestinationActionTypes.ENABLE_DISABLE_FAILURE:
      return set(ENABLE_STATUS, EntityStatus.loadingFailure, state);

    case DestinationActionTypes.SEND_TEST:
      return set(
        TEST_CONNECTION_STATUS,
        EntityStatus.loading,
        state
      );

    case DestinationActionTypes.SEND_TEST_SUCCESS:
      return set(
        TEST_CONNECTION_STATUS,
        EntityStatus.loadingSuccess,
        state
      );

    case DestinationActionTypes.SEND_TEST_FAILURE:
      return set(
        TEST_CONNECTION_STATUS,
        EntityStatus.loadingFailure,
        state
      );

      default:
        return state;
  }
}
