import { set, pipe } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { ProjectConstants } from 'app/entities/projects/project.model';
import {
  ProjectsFilterActions,
  ProjectsFilterActionTypes
} from './projects-filter.actions';

const UNASSIGNED_PROJECT_ID = ProjectConstants.UNASSIGNED_PROJECT_ID;
const ALL_RESOURCES_LABEL = ProjectConstants.ALL_RESOURCES_LABEL;
const ALL_PROJECTS_LABEL = ProjectConstants.ALL_PROJECTS_LABEL;
const MULTIPLE_PROJECTS_LABEL = ProjectConstants.MULTIPLE_PROJECTS_LABEL;

export interface ProjectsFilterOption {
  label: string;
  value: string;
  checked: boolean;
}

export interface ProjectsFilterState {
  options: ProjectsFilterOption[];
  optionsLoadingStatus: EntityStatus;
  selectionLabel: string;
  selectionCount: number;
  selectionCountVisible: boolean;
  selectionCountActive: boolean;
  dropdownCaretVisible: boolean;
}

export const projectsFilterInitialState: ProjectsFilterState = {
  options: [],
  optionsLoadingStatus: EntityStatus.notLoaded,
  selectionLabel: ALL_RESOURCES_LABEL,
  selectionCount: 0,
  selectionCountVisible: false,
  selectionCountActive: false,
  dropdownCaretVisible: false
};

export function projectsFilterReducer(
  state: ProjectsFilterState = projectsFilterInitialState,
  action: ProjectsFilterActions): ProjectsFilterState {

  switch (action.type) {

    case ProjectsFilterActionTypes.LOAD_OPTIONS: {
      return set('optionsLoadingStatus', EntityStatus.loading, state);
    }

    case ProjectsFilterActionTypes.LOAD_OPTIONS_SUCCESS: {
      return pipe(
        set('options', action.payload),
        set('optionsLoadingStatus', EntityStatus.loadingSuccess),
        set('selectionLabel', selectionLabel(action.payload)),
        set('selectionCount', selectionCount(action.payload)),
        set('selectionCountVisible', selectionCountVisible(action.payload)),
        set('selectionCountActive', selectionCountActive(action.payload)),
        set('dropdownCaretVisible', dropdownCaretVisible(action.payload))
      )(state) as ProjectsFilterState;
    }

    case ProjectsFilterActionTypes.LOAD_OPTIONS_FAILURE: {
      return set('optionsLoadingStatus', EntityStatus.loadingFailure, state);
    }

    case ProjectsFilterActionTypes.SAVE_OPTIONS: {
      return pipe(
        set('options', action.payload),
        set('selectionLabel', selectionLabel(action.payload)),
        set('selectionCount', selectionCount(action.payload)),
        set('selectionCountVisible', selectionCountVisible(action.payload)),
        set('selectionCountActive', selectionCountActive(action.payload)),
        set('dropdownCaretVisible', dropdownCaretVisible(action.payload))
      )(state) as ProjectsFilterState;
    }
  }

  return state;
}

function selectionLabel(options: ProjectsFilterOption[]): string {
  const checkedOptions = options.filter(o => o.checked);
  const projectOptions = options.filter(o => o.value !== UNASSIGNED_PROJECT_ID);
  const checkedProjects = projectOptions.filter(o => o.checked);
  const hasOneOption = options.length === 1;
  const hasOneChecked = checkedOptions.length === 1;
  const hasOneProjectChecked = checkedProjects.length === 1;
  const hasNoProjectsChecked = checkedProjects.length === 0;
  const hasAllProjectsChecked = checkedProjects.length === projectOptions.length;
  const hasSomeProjectsChecked = checkedProjects.length > 1 && !hasAllProjectsChecked;
  const hasUnassigned = options.filter(o => o.value === UNASSIGNED_PROJECT_ID).length === 1;
  const hasUnassignedChecked =
    options.filter(o => o.value === UNASSIGNED_PROJECT_ID && o.checked).length === 1;

  if (hasOneOption) {
    return options[0].label;
  }

  if (hasAllProjectsChecked && hasUnassignedChecked) {
    return ALL_RESOURCES_LABEL;
  }

  if (hasNoProjectsChecked && hasUnassigned && !hasUnassignedChecked) {
    return ALL_RESOURCES_LABEL;
  }

  if (hasOneChecked || hasOneProjectChecked) {
    return checkedOptions[0].label;
  }

  if (hasSomeProjectsChecked) {
    return MULTIPLE_PROJECTS_LABEL;
  }

  return ALL_PROJECTS_LABEL;
}

function selectionCount(options: ProjectsFilterOption[]): number {
  const checkedProjects = options.filter(o => o.checked && o.value !== UNASSIGNED_PROJECT_ID);
  return checkedProjects.length > 0 ? checkedProjects.length : options.length;
}

function selectionCountVisible(options: ProjectsFilterOption[]): boolean {
  const hasOnlyOneOption = options.length === 1;
  if (hasOnlyOneOption) {
    return false;
  }

  const checkedOptions = options.filter(o => o.checked);
  const projectOptions = options.filter(o => o.value !== UNASSIGNED_PROJECT_ID);
  const checkedProjects = projectOptions.filter(o => o.checked);
  const hasOnlyProjects = projectOptions.length === options.length;
  const hasNoneChecked = checkedOptions.length === 0;
  const hasAllChecked = checkedOptions.length === options.length;
  const hasSomeProjectsChecked = checkedProjects.length > 1;

  return hasOnlyProjects ?
    hasSomeProjectsChecked || hasNoneChecked :
    hasSomeProjectsChecked && !hasAllChecked;
}

function selectionCountActive(options: ProjectsFilterOption[]): boolean {
  const checkedOptions = options.filter(o => o.checked);
  return checkedOptions.length > 0;
}

function dropdownCaretVisible(options: ProjectsFilterOption[]): boolean {
  const hasOnlyOneOption = options.length === 1;
  return !hasOnlyOneOption;
}
