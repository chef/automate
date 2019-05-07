import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';
import { ProjectsFilterOption, ProjectsFilterOptionTuple } from './projects-filter.reducer';

export enum ProjectsFilterActionTypes {
  LOAD_OPTIONS         = 'PROJECTS_FILTER::LOAD_OPTIONS',
  LOAD_OPTIONS_SUCCESS = 'PROJECTS_FILTER::LOAD_OPTIONS::SUCCESS',
  LOAD_OPTIONS_FAILURE = 'PROJECTS_FILTER::LOAD_OPTIONS::FAILURE',
  SAVE_OPTIONS =         'PROJECTS_FILTER::SAVE_OPTIONS'
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

export type ProjectsFilterActions =
  | LoadOptions
  | LoadOptionsSuccess
  | LoadOptionsFailure
  | SaveOptions;
