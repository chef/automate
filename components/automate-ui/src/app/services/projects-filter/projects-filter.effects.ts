import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { of as observableOf } from 'rxjs';
import { switchMap, catchError, map, tap } from 'rxjs/operators';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { ProjectsFilterOption, ProjectsFilterOptionTuple } from './projects-filter.reducer';
import { ProjectsFilterService } from './projects-filter.service';
import { ProjectsFilterRequests, AuthorizedProjectsResponse } from './projects-filter.requests';
import {
  ProjectsFilterActionTypes,
  LoadOptions,
  LoadOptionsSuccess,
  LoadOptionsFailure,
  SaveOptions
} from './projects-filter.actions';

@Injectable()
export class ProjectsFilterEffects {
  constructor(
    private actions$: Actions,
    private projectsFilter: ProjectsFilterService,
    private requests: ProjectsFilterRequests

  ) { }

  @Effect()
  loadOptions$ = this.actions$.pipe(
    ofType<LoadOptions>(ProjectsFilterActionTypes.LOAD_OPTIONS),
    switchMap(() => {
      return this.requests.fetchOptions().pipe(
        map((fetched: AuthorizedProjectsResponse) => {
          const converted = convertResponse(fetched.projects);
          const restored = this.projectsFilter.restoreOptions() || [];
          return new LoadOptionsSuccess(<ProjectsFilterOptionTuple>{
            fetched: converted,
            restored: restored
          });
        }),
        catchError((error: HttpErrorResponse) => observableOf(new LoadOptionsFailure(error))));
    }));

  @Effect({ dispatch: false })
  saveOptions$ = this.actions$.pipe(
    ofType<SaveOptions>(ProjectsFilterActionTypes.SAVE_OPTIONS),
    tap(({ payload }) => this.projectsFilter.storeOptions(payload))
  );
}

export function convertResponse(authorizedProjects: string[]): ProjectsFilterOption[] {
  let convertedProjects: ProjectsFilterOption[];
  authorizedProjects.forEach(project => {
      let option = <ProjectsFilterOption>{
          label: project,
          value: project,
          checked: false,
      }
      convertedProjects.push(option)
  });
  return convertedProjects
}
