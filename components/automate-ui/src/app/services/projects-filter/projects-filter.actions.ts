import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { ProjectsFilterOption, ProjectsFilterOptionTuple } from './projects-filter.reducer';

export enum ProjectsFilterActionTypes {
  INIT_OPTIONS           = 'PROJECTS_FILTER::INIT_OPTIONS',
  INIT_OPTIONS_SUCCESS   = 'PROJECTS_FILTER::INIT_OPTIONS::SUCCESS',
  LOAD_OPTIONS           = 'PROJECTS_FILTER::LOAD_OPTIONS',
  LOAD_OPTIONS_SUCCESS   = 'PROJECTS_FILTER::LOAD_OPTIONS::SUCCESS',
  LOAD_OPTIONS_FAILURE   = 'PROJECTS_FILTER::LOAD_OPTIONS::FAILURE',
  SAVE_OPTIONS           = 'PROJECTS_FILTER::SAVE_OPTIONS',
  UPDATE_SELECTION_COUNT = 'PROJECTS_FILTER::UPDATE_SELECTION_COUNT'
}

export class InitOptions implements Action {
  readonly type = ProjectsFilterActionTypes.INIT_OPTIONS;
}

export class InitOptionsSuccess implements Action {
  readonly type = ProjectsFilterActionTypes.INIT_OPTIONS_SUCCESS;
  constructor(public payload: ProjectsFilterOptionTuple) { }
}

export class LoadOptions implements Action {
  readonly type = ProjectsFilterActionTypes.LOAD_OPTIONS;
}

export class LoadOptionsSuccess implements Action {
  readonly type = ProjectsFilterActionTypes.LOAD_OPTIONS_SUCCESS;
  constructor(public payload: ProjectsFilterOptionTuple) { }
}

export class LoadOptionsFailure implements Action {
  readonly type = ProjectsFilterActionTypes.LOAD_OPTIONS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class SaveOptions implements Action {
  readonly type = ProjectsFilterActionTypes.SAVE_OPTIONS;
  constructor(public payload: ProjectsFilterOption[]) { }
}

export class UpdateSelectionCount implements Action {
  readonly type = ProjectsFilterActionTypes.UPDATE_SELECTION_COUNT;
  constructor(public payload: ProjectsFilterOption[]) { }
}

export type ProjectsFilterActions =
  | InitOptions
  | InitOptionsSuccess
  | LoadOptions
  | LoadOptionsSuccess
  | LoadOptionsFailure
  | SaveOptions
  | UpdateSelectionCount;
