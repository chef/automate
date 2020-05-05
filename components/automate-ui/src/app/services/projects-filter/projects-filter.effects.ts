import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { interval as observableInterval, of as observableOf, Observable } from 'rxjs';
import { catchError, mergeMap, map, tap } from 'rxjs/operators';
import { intersectionBy } from 'lodash/fp';


import { Project } from 'app/entities/projects/project.model';
import { ProjectsFilterOption } from './projects-filter.reducer';
import { ProjectsFilterService } from './projects-filter.service';
import { ProjectsFilterRequests, AuthorizedProjectsResponse } from './projects-filter.requests';
import {
  ProjectsFilterActionTypes,
  ProjectsFilterActions,
  InitOptionsSuccess,
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

  private POLLING_INTERVAL_IN_SECONDS = 120; // 2 minutes

  @Effect()
  latestOptions$ = observableInterval(1000 * this.POLLING_INTERVAL_IN_SECONDS).pipe(
    mergeMap(this.loadOptionsAction$()));

  // Fast-initialize project filter just from local storage
  @Effect()
  initOptions$ = this.actions$.pipe(
    ofType<LoadOptions>(ProjectsFilterActionTypes.INIT_OPTIONS),
    map(() => {
      return new InitOptionsSuccess({
        fetched: [],
        restored: this.projectsFilter.restoreOptions() || []
      });
    })
  );

  @Effect()
  loadOptions$ = this.actions$.pipe(
    ofType<LoadOptions>(ProjectsFilterActionTypes.LOAD_OPTIONS),
    mergeMap(this.loadOptionsAction$()));

  @Effect({ dispatch: false })
  saveOptions$ = this.actions$.pipe(
    ofType<SaveOptions>(ProjectsFilterActionTypes.SAVE_OPTIONS),
    tap(({ payload }) => this.projectsFilter.storeOptions(payload))
  );

  private loadOptionsAction$(): () => Observable<ProjectsFilterActions> {
    return () => this.requests.fetchOptions().pipe(
      map((resp: AuthorizedProjectsResponse) => {
        const fetched = this.convertResponse(resp.projects);
        let restored = this.projectsFilter.restoreOptions() || [];

        // throw out any localstoreage keys that no longer exist.
        // intersectionBy choses results from the first array so
        // restored's checked results are preserved.
        restored = intersectionBy('value', restored, fetched);

        // don't reload the page, but persist to localstorage
        this.projectsFilter.updateLocalstorage(restored);

        return new LoadOptionsSuccess({
          fetched,
          restored
        });
      }),
      catchError((error: HttpErrorResponse) => observableOf(new LoadOptionsFailure(error))));
  }

  private convertResponse(authorizedProjects: Project[]): ProjectsFilterOption[] {
    return authorizedProjects.map(
      project => ({
        label: project.name,
        value: project.id,
        type: project.type,
        checked: false
      }));
  }
}

