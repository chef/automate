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
  const checked = options.filter(o => o.checked);

  if (options.length === 1) {
    return options[0].label;
  }

  if (checked.length === 1) {
    return checked[0].label;
  }

  if (checked.length === options.length) {
    return 'All projects';
  }

  if (checked.length > 1) {
    return 'Multiple projects';
  }

  return 'All resources';
}

function selectionCount(options: ProjectsFilterOption[]): number {
  const checked = options.filter(o => o.checked);
  return checked.length > 0 ? checked.length : options.length;
}

function selectionCountVisible(options: ProjectsFilterOption[]): boolean {
  const checked = options.filter(o => o.checked);
  const hasMultipleOptions = options.length > 1;
  const hasNoneSelected = checked.length === 0;
  const hasMultipleSelected = checked.length > 1 && checked.length < options.length;
  const hasLessThanAllSelected = checked.length < options.length;
  return hasMultipleOptions && (hasNoneSelected || (hasMultipleSelected && hasLessThanAllSelected));
}

function selectionCountActive(options: ProjectsFilterOption[]): boolean {
  const checked = options.filter(o => o.checked);
  return checked.length > 0;
}
