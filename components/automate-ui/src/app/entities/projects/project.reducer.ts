import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { HttpErrorResponse } from '@angular/common/http';
import { set, pipe, unset, mapKeys, camelCase } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { ProjectActionTypes, ProjectActions } from './project.actions';
import { Project } from './project.model';

export type ApplyRulesStatusState = 'not_running' | 'running';

export interface ApplyRulesStatus {
  state: ApplyRulesStatusState;
  estimatedTimeComplete: string;
  percentageComplete: number;
  failed: boolean;
  failureMessage: string;
}

export interface ProjectEntityState extends EntityState<Project> {
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  createStatus: EntityStatus;
  createError: HttpErrorResponse;
  updateStatus: EntityStatus;
  deleteStatus: EntityStatus;
  applyRulesStatus: ApplyRulesStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_STATUS = 'getStatus';
const CREATE_STATUS = 'createStatus';
const CREATE_ERROR = 'createError';
const DELETE_STATUS = 'deleteStatus';
const UPDATE_STATUS = 'updateStatus';
const APPLY_RULES_STATUS = 'applyRulesStatus';

export const projectEntityAdapter: EntityAdapter<Project> = createEntityAdapter<Project>();

const initialApplyRulesStatus: ApplyRulesStatus = {
  state: 'not_running',
  estimatedTimeComplete: '0001-01-01T00:00:00Z',
  percentageComplete: 0,
  failed: false,
  failureMessage: ''
};

export const ProjectEntityInitialState: ProjectEntityState = projectEntityAdapter.getInitialState({
  getAllStatus: EntityStatus.notLoaded,
  getStatus: EntityStatus.notLoaded,
  createStatus: EntityStatus.notLoaded,
  createError: null,
  deleteStatus: EntityStatus.notLoaded,
  updateStatus: EntityStatus.notLoaded,
  applyRulesStatus: initialApplyRulesStatus
});

export function projectEntityReducer(
  state: ProjectEntityState = ProjectEntityInitialState,
  action: ProjectActions): ProjectEntityState {

  switch (action.type) {
    case ProjectActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading, state);

    case ProjectActionTypes.GET_ALL_SUCCESS:
      return set(GET_ALL_STATUS, EntityStatus.loadingSuccess,
        projectEntityAdapter.addAll(action.payload.projects, state));

    case ProjectActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    case ProjectActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading, state);

    case ProjectActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        projectEntityAdapter.addOne(action.payload.project, state));

    case ProjectActionTypes.GET_FAILURE:
      return set(GET_STATUS, EntityStatus.loadingFailure, state);

    case ProjectActionTypes.CREATE:
      return set(CREATE_STATUS, EntityStatus.loading, state);

    case ProjectActionTypes.CREATE_SUCCESS:
      return pipe(
        set(CREATE_STATUS, EntityStatus.loadingSuccess),
        unset(CREATE_ERROR)
      )(projectEntityAdapter.addOne(action.payload.project, state)) as ProjectEntityState;

    case ProjectActionTypes.CREATE_FAILURE:
      return pipe(
        set(CREATE_STATUS, EntityStatus.loadingFailure),
        set(CREATE_ERROR, action.payload)
      )(state) as ProjectEntityState;

    case ProjectActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case ProjectActionTypes.DELETE_SUCCESS:
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        projectEntityAdapter.removeOne(action.payload.id, state));

    case ProjectActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);

    case ProjectActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case ProjectActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        projectEntityAdapter.updateOne({
          id: action.payload.project.id,
          changes: action.payload.project
        }, state));

    case ProjectActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

    case ProjectActionTypes.GET_APPLY_RULES_STATUS_SUCCESS:
      return set(APPLY_RULES_STATUS, mapKeys(key => camelCase(key), action.payload), state);

    default:
      return state;
  }
}
