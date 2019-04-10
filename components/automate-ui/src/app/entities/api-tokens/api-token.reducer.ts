import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { pipe, set, unset } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { ApiToken } from './api-token.model';
import { ApiTokenActionTypes, ApiTokenActions } from './api-token.actions';

export interface ApiTokenEntityState extends EntityState<ApiToken> {
  status: EntityStatus;
  saveStatus: EntityStatus;
  saveError: HttpErrorResponse;
  deleteStatus: EntityStatus;
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
  tokenToDelete: null | ApiToken;
}

// reusable names of the above properties:
// TODO (tc): Rename this to GET_ALL_STATUS
const STATUS = 'status';
const SAVE_STATUS = 'saveStatus';
const SAVE_ERROR = 'saveError';
const DELETE_STATUS = 'deleteStatus';
const GET_STATUS = 'getStatus';
const UPDATE_STATUS = 'updateStatus';

export const apiTokenEntityAdapter: EntityAdapter<ApiToken> = createEntityAdapter<ApiToken>();

export const ApiTokenEntityInitialState: ApiTokenEntityState =
  apiTokenEntityAdapter.getInitialState({
    status: EntityStatus.notLoaded,
    saveStatus: EntityStatus.notLoaded,
    saveError: null,
    deleteStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded,
    updateStatus: EntityStatus.notLoaded,
    tokenToDelete: null
  });

export function apiTokenEntityReducer(
  state: ApiTokenEntityState = ApiTokenEntityInitialState,
  action: ApiTokenActions): ApiTokenEntityState {

  switch (action.type) {

    case ApiTokenActionTypes.GET_ALL: {
      return set(STATUS, EntityStatus.loading, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.GET_ALL_SUCCESS: {
      return set(
        STATUS,
        EntityStatus.loadingSuccess,
        apiTokenEntityAdapter.addAll(action.payload.tokens, state)
      ) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.GET_ALL_FAILURE: {
      return set(STATUS, EntityStatus.loadingFailure, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.GET: {
      return set(GET_STATUS, EntityStatus.loading, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.GET_SUCCESS: {
      return set(
        GET_STATUS,
        EntityStatus.loadingSuccess,
        apiTokenEntityAdapter.addOne(action.payload, state)
      ) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.GET_FAILURE: {
      return set(GET_STATUS, EntityStatus.loadingFailure, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.UPDATE: {
      return set(UPDATE_STATUS, EntityStatus.loading, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.UPDATE_SUCCESS: {
      return set(
        UPDATE_STATUS,
        EntityStatus.loadingSuccess,
        apiTokenEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload
        }, state)
      ) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.UPDATE_FAILURE: {
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.CREATE: {
      return set(SAVE_STATUS, EntityStatus.loading, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.CREATE_SUCCESS: {
      return pipe(
        unset(SAVE_ERROR),
        set(SAVE_STATUS, EntityStatus.loadingSuccess)
      )(apiTokenEntityAdapter.addOne(action.payload, state)) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.CREATE_FAILURE: {
      return pipe(
        set(SAVE_ERROR, action.payload),
        set(SAVE_STATUS, EntityStatus.loadingFailure)
      )(state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.TOGGLE_SUCCESS: {
      return apiTokenEntityAdapter.updateOne({
          id: action.payload.id,
          changes: {
            active: action.payload.active
          }
        }, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.DELETE: {
      return set(DELETE_STATUS, EntityStatus.loading, state) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.DELETE_SUCCESS: {
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        apiTokenEntityAdapter.removeOne(action.payload.id, state)) as ApiTokenEntityState;
    }

    case ApiTokenActionTypes.DELETE_FAILURE: {
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state) as ApiTokenEntityState;
    }
  }

  return state;
}
