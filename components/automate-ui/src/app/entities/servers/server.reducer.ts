import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { pipe, set, unset } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { ServerActionTypes, ServerActions } from './server.actions';
import { Server } from './server.model';

export interface ServerEntityState extends EntityState<Server> {
  getAllStatus: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';
const UPDATE_STATUS = 'updateStatus';
const GET_STATUS = 'getStatus';
const DELETE_STATUS = 'deleteStatus';

export const serverEntityAdapter: EntityAdapter<Server> = createEntityAdapter<Server>();

export const ServerEntityInitialState: ServerEntityState =
  serverEntityAdapter.getInitialState({
    getAllStatus: EntityStatus.notLoaded,
    saveStatus: EntityStatus.notLoaded,
    saveError: null,
    updateStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded,
    deleteStatus: EntityStatus.notLoaded
  });

export function serverEntityReducer(
  state: ServerEntityState = ServerEntityInitialState,
  action: ServerActions): ServerEntityState {

  switch (action.type) {

    case ServerActionTypes.GET_ALL: {
      return set(GET_ALL_STATUS, EntityStatus.loading, state);
    }

    case ServerActionTypes.GET_ALL_SUCCESS: {
      return set(
        GET_ALL_STATUS,
        EntityStatus.loadingSuccess,
        serverEntityAdapter.setAll(action.payload.servers, state)
      ) as ServerEntityState;
    }

    case ServerActionTypes.GET_ALL_FAILURE: {
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state) as ServerEntityState;
    }

    case ServerActionTypes.GET: {
      return set(
        GET_STATUS,
        EntityStatus.loading,
        serverEntityAdapter.removeAll(state)
      ) as ServerEntityState;
    }

    case ServerActionTypes.GET_SUCCESS: {
      return set(
        GET_STATUS,
        EntityStatus.loadingSuccess,
        serverEntityAdapter.addOne(action.payload.server, state)
      ) as ServerEntityState;
    }

    case ServerActionTypes.GET_FAILURE: {
      return set(GET_STATUS, EntityStatus.loadingFailure, state) as ServerEntityState;
    }

    case ServerActionTypes.CREATE: {
      return set(SAVE_STATUS, EntityStatus.loading, state) as ServerEntityState;
    }

    case ServerActionTypes.CREATE_SUCCESS: {
      return pipe(
        unset(SAVE_ERROR),
        set(SAVE_STATUS, EntityStatus.loadingSuccess)
      )(serverEntityAdapter.addOne(action.payload.server, state)) as ServerEntityState;
    }

    case ServerActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as ServerEntityState;
    }

    case ServerActionTypes.DELETE: {
      return set(DELETE_STATUS, EntityStatus.loading, state);
    }

    case ServerActionTypes.DELETE_SUCCESS: {
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        serverEntityAdapter.removeOne(action.payload.id, state));
    }

    case ServerActionTypes.DELETE_FAILURE: {
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);
    }

    case ServerActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case ServerActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        serverEntityAdapter.updateOne({
          id: action.payload.server.id,
          changes: action.payload.server
        }, state));

    case ServerActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

  }

  return state;
}
