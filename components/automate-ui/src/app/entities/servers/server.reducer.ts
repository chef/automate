import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { pipe, set, unset } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { ServerActionTypes, ServerActions } from './server.actions';
import { MigrationStatus, Server, User } from './server.model';
import { ValidateWebUIKeyResponse } from './server.requests';

export interface ServerEntityState extends EntityState<Server> {
  getAllStatus: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
  getUsers: User[];
  getUsersStatus: EntityStatus;
  updateWebUIKeyStatus: EntityStatus;
  getvalidateWebUIKeyStatus: ValidateWebUIKeyResponse;
  validateWebUIKeyStatus: EntityStatus;
  getMigrationStatus: MigrationStatus;
  migrationStatus: EntityStatus;
}

const GET_ALL_STATUS             = 'getAllStatus';
const SAVE_STATUS                = 'saveStatus';
const SAVE_ERROR                 = 'saveError';
const UPDATE_STATUS              = 'updateStatus';
const GET_STATUS                 = 'getStatus';
const DELETE_STATUS              = 'deleteStatus';
const GET_USERS_STATUS           = 'getUsersStatus';
const UPDATE_WEB_UI_KEY_STATUS   = 'updateWebUIKeyStatus';
const VALIDATE_WEB_UI_KEY_STATUS = 'validateWebUIKeyStatus';
const GET_MIGRATION_STATUS       = 'migrationStatus';

export const serverEntityAdapter: EntityAdapter<Server> = createEntityAdapter<Server>();

export const ServerEntityInitialState: ServerEntityState =
  serverEntityAdapter.getInitialState({
    getAllStatus: EntityStatus.notLoaded,
    saveStatus: EntityStatus.notLoaded,
    saveError: null,
    updateStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded,
    deleteStatus: EntityStatus.notLoaded,
    getUsers: null,
    getUsersStatus: EntityStatus.notLoaded,
    updateWebUIKeyStatus: EntityStatus.notLoaded,
    getvalidateWebUIKeyStatus: null,
    validateWebUIKeyStatus: EntityStatus.notLoaded,
    getMigrationStatus: null,
    migrationStatus: EntityStatus.notLoaded
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

    case ServerActionTypes.GET_USERS:
      return set(
        GET_USERS_STATUS,
        EntityStatus.loading,
        serverEntityAdapter.removeAll(state)
      ) as ServerEntityState;

    case ServerActionTypes.GET_USERS_SUCCESS:
      return pipe(
        set(GET_USERS_STATUS, EntityStatus.loadingSuccess),
        set('getUsers', action.payload || [])
      )(state) as ServerEntityState;

    case ServerActionTypes.GET_USERS_FAILURE:
      return set(
        GET_USERS_STATUS, EntityStatus.loadingFailure, state);

    case ServerActionTypes.UPDATE_WEB_UI_KEY:
      return set(UPDATE_WEB_UI_KEY_STATUS, EntityStatus.loading, state);

    case ServerActionTypes.UPDATE_WEB_UI_KEY_SUCCESS:
      return set(UPDATE_WEB_UI_KEY_STATUS, EntityStatus.loadingSuccess, state);

    case ServerActionTypes.UPDATE_WEB_UI_KEY_FAILURE:
      return set(UPDATE_WEB_UI_KEY_STATUS, EntityStatus.loadingFailure, state);

    case ServerActionTypes.VALIDATE_WEB_UI_KEY:
      return set(VALIDATE_WEB_UI_KEY_STATUS, EntityStatus.loading, state);

    case ServerActionTypes.VALIDATE_WEB_UI_KEY_SUCCESS:
      return pipe(
        set(VALIDATE_WEB_UI_KEY_STATUS, EntityStatus.loadingSuccess),
        set('getvalidateWebUIKeyStatus', action.payload || {})
      )(state) as ServerEntityState;

    case ServerActionTypes.VALIDATE_WEB_UI_KEY_FAILURE:
      return set(VALIDATE_WEB_UI_KEY_STATUS, EntityStatus.loadingFailure, state);

    case ServerActionTypes.GET_MIGRATION_STATUS:
      return set(GET_MIGRATION_STATUS, EntityStatus.loading, state);

    case ServerActionTypes.GET_MIGRATION_STATUS_SUCCESS:
      return pipe(
        set(GET_MIGRATION_STATUS, EntityStatus.loadingSuccess),
        set('getMigrationStatus', action.payload || {})
      )(state) as ServerEntityState;

    case ServerActionTypes.GET_MIGRATION_STATUS_FAILURE:
      return set(GET_MIGRATION_STATUS, EntityStatus.loadingFailure, state);

  }

  return state;
}
