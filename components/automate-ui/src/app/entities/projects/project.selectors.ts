
import { createSelector, createFeatureSelector } from '@ngrx/store';

import { routeParams } from 'app/route.selectors';
import { ProjectEntityState, projectEntityAdapter } from './project.reducer';

export const projectState = createFeatureSelector<ProjectEntityState>('projects');

export const {
  selectAll: allProjects,
  selectEntities: projectEntities
} = projectEntityAdapter.getSelectors(projectState);

export const getAllStatus = createSelector(
  projectState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
  projectState,
  (state) => state.getStatus
);

export const createStatus = createSelector(
  projectState,
  (state) => state.createStatus
);

export const createError = createSelector(
  projectState,
  (state) => state.createError
);

export const updateStatus = createSelector(
  projectState,
  (state) => state.updateStatus
);

export const projectFromRoute = createSelector(
  projectEntities,
  routeParams,
  (state, { id }) => state[id]
);

export const applyRulesStatus = createSelector(
  projectState, state => state.applyRulesStatus
);
