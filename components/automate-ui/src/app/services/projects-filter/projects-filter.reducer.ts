import { set, pipe, find } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { ChefSorters } from 'app/helpers/auth/sorter';
import { ProjectConstants } from 'app/entities/projects/project.model';
import {
  ProjectsFilterActions,
  ProjectsFilterActionTypes
} from './projects-filter.actions';

const {
  UNASSIGNED_PROJECT_ID,
  ALL_PROJECTS_LABEL,
  MULTIPLE_PROJECTS_LABEL
} = ProjectConstants;

export interface ProjectsFilterOption {
  label: string;
  value: string;
  type:  string;
  checked: boolean;
}

export interface ProjectsFilterOptionTuple {
  fetched: ProjectsFilterOption[];
  restored: ProjectsFilterOption[];
}

export interface ProjectsFilterState {
  options: ProjectsFilterOption[];
  optionsLoadingStatus: EntityStatus;
  selectionLabel: string;
  selectionCount: number;
  selectionCountVisible: boolean;
  selectionCountActive: boolean;
  dropdownCaretVisible: boolean;
  filterVisible: boolean;
}

export const projectsFilterInitialState: ProjectsFilterState = {
  options: [],
  optionsLoadingStatus: EntityStatus.notLoaded,
  selectionLabel: ALL_PROJECTS_LABEL,
  selectionCount: 0,
  selectionCountVisible: false,
  selectionCountActive: false,
  dropdownCaretVisible: false,
  filterVisible: false
};

export function projectsFilterReducer(
  state: ProjectsFilterState = projectsFilterInitialState,
  action: ProjectsFilterActions): ProjectsFilterState {

  switch (action.type) {

    case ProjectsFilterActionTypes.LOAD_OPTIONS: {
      return set('optionsLoadingStatus', EntityStatus.loading, state);
    }

    case ProjectsFilterActionTypes.INIT_OPTIONS_SUCCESS: {
      const sortedOptions = sortOptions(action.payload.restored);
      return setDropdownProperties(sortedOptions, state);
    }

    case ProjectsFilterActionTypes.LOAD_OPTIONS_SUCCESS: {
      const mergedOptions = mergeOptions(action.payload.fetched, action.payload.restored);
      const sortedOptions = sortOptions(mergedOptions);
      return setDropdownProperties(sortedOptions, state);
    }

    case ProjectsFilterActionTypes.LOAD_OPTIONS_FAILURE: {
      return set('optionsLoadingStatus', EntityStatus.loadingFailure, state);
    }

    case ProjectsFilterActionTypes.SAVE_OPTIONS: {
      // merge any added or removed options with the newly saved values
      const mergedOptions = mergeOptions(state.options, action.payload);
      const sortedOptions = sortOptions(mergedOptions);
      return pipe(
        set('options', sortedOptions),
        set('selectionLabel', selectionLabel(sortedOptions)),
        set('selectionCount', selectionCount(sortedOptions)),
        set('selectionCountVisible', selectionCountVisible(sortedOptions)),
        set('selectionCountActive', selectionCountActive(sortedOptions)),
        set('dropdownCaretVisible', dropdownCaretVisible(sortedOptions))
      )(state) as ProjectsFilterState;
    }

    case ProjectsFilterActionTypes.UPDATE_SELECTION_COUNT: {
      // merge any added or removed options with the newly saved values
      const mergedOptions = mergeOptions(state.options, action.payload);
      const sortedOptions = sortOptions(mergedOptions);
      return pipe(
        set('selectionLabel', selectionLabel(sortedOptions)),
        set('selectionCount', selectionCount(sortedOptions)),
        set('selectionCountVisible', selectionCountVisible(sortedOptions)),
        set('selectionCountActive', selectionCountActive(sortedOptions))
      )(state) as ProjectsFilterState;
    }
  }

  return state;
}

function setDropdownProperties(
  sortedOptions: ProjectsFilterOption[], state: ProjectsFilterState): ProjectsFilterState {
  return pipe(
    set('options', sortedOptions),
    set('optionsLoadingStatus', EntityStatus.loadingSuccess),
    set('selectionLabel', selectionLabel(sortedOptions)),
    set('selectionCount', selectionCount(sortedOptions)),
    set('selectionCountVisible', selectionCountVisible(sortedOptions)),
    set('selectionCountActive', selectionCountActive(sortedOptions)),
    set('dropdownCaretVisible', dropdownCaretVisible(sortedOptions)),
    set('filterVisible', filterVisible(sortedOptions)
    ))(state) as ProjectsFilterState;
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
    return ALL_PROJECTS_LABEL;
  }

  if (hasNoProjectsChecked && hasUnassigned && !hasUnassignedChecked) {
    return ALL_PROJECTS_LABEL;
  }

  if (hasOneChecked || hasOneProjectChecked) {
    // We need to take the last item in the array because unassigned is first
    // when unassigned and a single project are checked at the same time
    return checkedOptions[checkedOptions.length - 1].label;
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

function filterVisible(options: ProjectsFilterOption[]): boolean {
  const hasSomePermissions = options.length > 0;
  const hasOnlyUnassignedPermission = options.length === 1 &&
    options[0].value === UNASSIGNED_PROJECT_ID;
  return hasSomePermissions && !hasOnlyUnassignedPermission;
}

// Grab previously saved options from localstorage (restored) and,
// if any has the same value as one of the newly fetched options (fetched),
// merge its current checked status with the fetched option
// to create the final list of available options.
function mergeOptions(
  fetched: ProjectsFilterOption[], restored: ProjectsFilterOption[]): ProjectsFilterOption[] {
  return fetched.map(fetchedOpt => {
    const restoredOpt = find(['value', fetchedOpt.value], restored);
    return restoredOpt ? { ...fetchedOpt, checked: restoredOpt.checked } : fetchedOpt;
  });
}

function sortOptions(options: ProjectsFilterOption[]): ProjectsFilterOption[] {
  // Sort all except unassigned, which should always be first
  const sorted = options.filter(o => o.value !== UNASSIGNED_PROJECT_ID);
  ChefSorters.naturalSort(sorted, 'label');

  const unassignedProject = find(['value', UNASSIGNED_PROJECT_ID], options);
  if (unassignedProject) {
    sorted.unshift(unassignedProject);
  }

  return sorted;
}
