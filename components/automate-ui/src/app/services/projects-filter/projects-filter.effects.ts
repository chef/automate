import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { interval as observableInterval, of as observableOf, Observable } from 'rxjs';
import { catchError, mergeMap, map, tap } from 'rxjs/operators';

import { Project } from 'app/entities/projects/project.model';
import { ProjectRequests } from 'app/entities/projects/project.requests';
import { GetProjectsSuccessPayload } from 'app/entities/projects/project.actions';
import { ProjectsFilterOption, ProjectsFilterOptionTuple } from './projects-filter.reducer';
import { ProjectsFilterService } from './projects-filter.service';
import {
  ProjectsFilterActionTypes,
  ProjectsFilterActions,
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
    private requests: ProjectRequests
  ) { }

  private POLLING_INTERVAL_IN_SECONDS = 120; // 2 minutes

  @Effect()
  latestOptions$ = observableInterval(1000 * this.POLLING_INTERVAL_IN_SECONDS)
    .pipe(mergeMap(this.loadOptionsAction$()));

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
    return () => this.requests.getProjects(true).pipe(
      map((fetched: GetProjectsSuccessPayload) => {
        const converted = this.convertResponse(fetched.projects);
        const restored = this.projectsFilter.restoreOptions() || [];
        return new LoadOptionsSuccess(<ProjectsFilterOptionTuple>{
          fetched: converted,
          restored: restored
        });
      }),
      catchError((error: HttpErrorResponse) => observableOf(new LoadOptionsFailure(error))));
  }

  private convertResponse(authorizedProjects: Project[]): ProjectsFilterOption[] {
    return authorizedProjects.map(
      project => <ProjectsFilterOption>{
        label: project.name,
        value: project.id,
        checked: false
      });
  }
}

