import { set, pipe } from 'lodash/fp';
import { LoadingStatus } from 'app/types/types';
import {
  ProjectsFilterActions,
  ProjectsFilterActionTypes
} from './projects-filter.actions';

export interface ProjectsFilterOption {
  label: string;
  value: string;
  checked: boolean;
}

export interface ProjectsFilterState {
  options: ProjectsFilterOption[];
  optionsLoadingStatus: LoadingStatus;
  selectionLabel: string;
  selectionCount: number;
  selectionCountVisible: boolean;
  selectionCountActive: boolean;
}

export const projectsFilterInitialState: ProjectsFilterState = {
  options: [],
  optionsLoadingStatus: LoadingStatus.notLoaded,
  selectionLabel: 'All resources',
  selectionCount: 0,
  selectionCountVisible: false,
  selectionCountActive: false
};

export function projectsFilterReducer(
  state: ProjectsFilterState = projectsFilterInitialState,
  action: ProjectsFilterActions): ProjectsFilterState {

  switch (action.type) {

    case ProjectsFilterActionTypes.LOAD_OPTIONS: {
      return set('optionsLoadingStatus', LoadingStatus.loading, state);
    }

    case ProjectsFilterActionTypes.LOAD_OPTIONS_SUCCESS: {
      return pipe(
        set('options', action.payload),
        set('optionsLoadingStatus', LoadingStatus.loadingSuccess),
        set('selectionLabel', selectionLabel(action.payload)),
        set('selectionCount', selectionCount(action.payload)),
        set('selectionCountVisible', selectionCountVisible(action.payload)),
        set('selectionCountActive', selectionCountActive(action.payload))
      )(state) as ProjectsFilterState;
    }

    case ProjectsFilterActionTypes.LOAD_OPTIONS_FAILURE: {
      return set('optionsLoadingStatus', LoadingStatus.loadingFailure, state);
    }

    case ProjectsFilterActionTypes.SAVE_OPTIONS: {
      return pipe(
        set('options', action.payload),
        set('selectionLabel', selectionLabel(action.payload)),
        set('selectionCount', selectionCount(action.payload)),
        set('selectionCountVisible', selectionCountVisible(action.payload)),
        set('selectionCountActive', selectionCountActive(action.payload))
      )(state) as ProjectsFilterState;
    }
  }

  return state;
}

function selectionLabel(options: ProjectsFilterOption[]): string {
  const UNASSIGNED_PROJECT = '(unassigned)';
  const checkedOptions = options.filter(o => o.checked);
  const projectOptions = options.filter(o => o.value !== UNASSIGNED_PROJECT);
  const checkedProjects = projectOptions.filter(o => o.checked);
  const hasOneOption = options.length === 1;
  const hasOneChecked = checkedOptions.length === 1;
  const hasOneProjectChecked = checkedProjects.length === 1;
  const hasNoProjectsChecked = checkedProjects.length === 0;
  const hasAllProjectsChecked = checkedProjects.length === projectOptions.length;
  const hasSomeProjectsChecked = checkedProjects.length > 1 && !hasAllProjectsChecked;
  const hasUnassignedChecked =
    options.filter(o => o.value === UNASSIGNED_PROJECT && o.checked).length === 1;
  const unassignedAvailable = options.filter(o => o.value === UNASSIGNED_PROJECT).length === 1;

  if (hasOneOption) {
    return options[0].label;
  }

  if (hasOneChecked) {
    return checkedOptions[0].label;
  }

  if (hasOneProjectChecked) {
    return checkedProjects[0].label;
  }

  if ((hasAllProjectsChecked && (!hasUnassignedChecked || !unassignedAvailable))
    || (hasNoProjectsChecked && !unassignedAvailable)){
      return 'All projects';
  }

  if (hasSomeProjectsChecked) {
    return 'Multiple projects';
  }

  return 'All resources';
}

function selectionCount(options: ProjectsFilterOption[]): number {
  const checkedProjects = options.filter(o => o.checked && o.value !== 'unassigned-resources');
  return checkedProjects.length > 0 ? checkedProjects.length : options.length;
}

function selectionCountVisible(options: ProjectsFilterOption[]): boolean {
  const hasOnlyOneOption = options.length === 1;
  if (hasOnlyOneOption) {
    return false;
  }

  const checkedOptions = options.filter(o => o.checked);
  const projectOptions = options.filter(o => o.value !== 'unassigned-resources');
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
