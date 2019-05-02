import { createSelector } from '@ngrx/store';

export const projectsFilterState = state => state.projectsFilter;

export const options = createSelector(projectsFilterState, state => state.options);

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
