import { HttpErrorResponse } from '@angular/common/http';
import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { difference, get, pipe, set } from 'lodash/fp';

import { EntityStatus } from '../entities';
import { TeamActionTypes, TeamActions } from './team.actions';
import { Team } from './team.model';

export interface TeamEntityState extends EntityState<Team> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  getUsersStatus: EntityStatus;
  createStatus: EntityStatus;
  createError: HttpErrorResponse;
  deleteStatus: EntityStatus;
  updateStatus: EntityStatus;
  addUsersStatus: EntityStatus;
  removeUsersStatus: EntityStatus;
  userIDs: string[];
}

export const teamEntityAdapter: EntityAdapter<Team> = createEntityAdapter<Team>();

export const TeamEntityInitialState: TeamEntityState = teamEntityAdapter.getInitialState({
  getAllStatus: EntityStatus.notLoaded,
  getStatus: EntityStatus.notLoaded,
  getUsersStatus: EntityStatus.notLoaded,
  createStatus: EntityStatus.notLoaded,
  createError: null,
  deleteStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded,
  addUsersStatus: EntityStatus.notLoaded,
  removeUsersStatus: EntityStatus.notLoaded,
  userIDs: []
});

export function teamEntityReducer(state: TeamEntityState = TeamEntityInitialState,
  action: TeamActions): TeamEntityState {

  switch (action.type) {

    case TeamActionTypes.GET_ALL: {
      return set('getAllStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.GET_ALL_SUCCESS: {
      return set(
        'getAllStatus',
        EntityStatus.loadingSuccess,
        teamEntityAdapter.addAll(action.payload.teams, state)
      ) as TeamEntityState;
    }

    case TeamActionTypes.GET_ALL_FAILURE: {
      return set('getAllStatus', EntityStatus.loadingFailure, state) as TeamEntityState;
    }

    case TeamActionTypes.GET_USERS: {
      return set('getUsersStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.GET_USERS_SUCCESS: {
      return pipe(
        set('getUsersStatus', EntityStatus.loadingSuccess),
        set('userIDs', action.payload.user_ids)
      )(state) as TeamEntityState;
    }

    case TeamActionTypes.GET_USERS_FAILURE: {
      return set('getUsersStatus', EntityStatus.loadingFailure, state) as TeamEntityState;
    }

    case TeamActionTypes.GET: {
      return set('getStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.GET_SUCCESS: {
      return set(
        'getStatus',
        EntityStatus.loadingSuccess,
        teamEntityAdapter.addOne(action.payload, state)
      ) as TeamEntityState;
    }

    case TeamActionTypes.GET_FAILURE: {
      return set('getStatus', EntityStatus.loadingFailure, state) as TeamEntityState;
    }

    case TeamActionTypes.CREATE: {
      return set('createStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.CREATE_SUCCESS: {
      return set(
        'createStatus',
        EntityStatus.loadingSuccess,
        teamEntityAdapter.addOne(action.payload, state)
      ) as TeamEntityState;
    }

    case TeamActionTypes.CREATE_FAILURE: {
      return pipe(
        set('createError', action.payload),
        set('createStatus', EntityStatus.loadingFailure)
        )(state) as TeamEntityState;
    }

    case TeamActionTypes.UPDATE: {
      return set('updateStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.UPDATE_SUCCESS: {
      return set(
        'updateStatus',
        EntityStatus.loadingSuccess,
        teamEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload
        }, state)
      ) as TeamEntityState;
    }

    case TeamActionTypes.UPDATE_FAILURE: {
      return set('updateStatus', EntityStatus.loadingFailure, state) as TeamEntityState;
    }

    case TeamActionTypes.DELETE: {
      return set('deleteStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.DELETE_SUCCESS: {
      return set(
        'deleteStatus',
        EntityStatus.loadingSuccess,
        teamEntityAdapter.removeOne(action.payload.id, state)
      ) as TeamEntityState;
    }

    case TeamActionTypes.DELETE_FAILURE: {
      return set('deleteStatus', EntityStatus.loadingFailure, state) as TeamEntityState;
    }

    case TeamActionTypes.ADD_USERS: {
      return set('addUsersStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.ADD_USERS_SUCCESS: {
      // get the current list and add the newly added user ids
      return pipe(
        set('addUsersStatus', EntityStatus.loadingSuccess),
        set('userIDs', [...state.userIDs, ...action.payload.user_ids])
      )(state) as TeamEntityState;
    }

    case TeamActionTypes.ADD_USERS_FAILURE: {
      return set('addUsersStatus', EntityStatus.loadingFailure, state) as TeamEntityState;
    }

    case TeamActionTypes.REMOVE_USERS: {
      return set('removeUsersStatus', EntityStatus.loading, state) as TeamEntityState;
    }

    case TeamActionTypes.REMOVE_USERS_SUCCESS: {
      // get the current list and filter the newly removed user ids
      return pipe(
        set('removeUsersStatus', EntityStatus.loadingSuccess),
        set('userIDs', difference(get('userIDs', state), action.payload.user_ids))
      )(state) as TeamEntityState;
    }

    case TeamActionTypes.REMOVE_USERS_FAILURE: {
      return set('removeUsersStatus', EntityStatus.loadingFailure, state) as TeamEntityState;
    }
  }

  return state;
}
