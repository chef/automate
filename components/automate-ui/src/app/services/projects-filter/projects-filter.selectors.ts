import { createSelector } from '@ngrx/store';

import { ProjectConstants } from 'app/entities/projects/project.model';
import { ProjectsFilterOption } from './projects-filter.reducer';

export const projectsFilterState = state => state.projectsFilter;

export const options = createSelector(projectsFilterState, state => state.options);

export const projectsAssignable = createSelector(projectsFilterState, state => {
  let projectOptions = state.options.filter((p: ProjectsFilterOption) => p.checked);

  // there is no project filter, populate all projects.
  if (projectOptions.length === 0) {
    projectOptions = state.options;
  }

  // remove the unassigned project if in filter as that cannot be an assignable value.
  // this might result in no assignable projects if they only selected the unassigned project.
  projectOptions = projectOptions.filter(p =>
    p.value !== ProjectConstants.UNASSIGNED_PROJECT_ID);

  return projectOptions;
});

export const optionsLoadingStatus = createSelector(projectsFilterState,
  state => state.optionsLoadingStatus);

export const selectionLabel = createSelector(projectsFilterState, state => state.selectionLabel);

export const selectionCount = createSelector(projectsFilterState, state => state.selectionCount);

export const selectionCountVisible = createSelector(projectsFilterState,
  state => state.selectionCountVisible);

export const selectionCountActive = createSelector(projectsFilterState,
  state => state.selectionCountActive);

export const dropdownCaretVisible = createSelector(projectsFilterState,
  state => state.dropdownCaretVisible);
