import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { of as observableOf } from 'rxjs';
import { find } from 'lodash/fp';
import { switchMap, catchError, map, tap } from 'rxjs/operators';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { ProjectsFilterOption } from './projects-filter.reducer';
import { ProjectsFilterService } from './projects-filter.service';
import {
  ProjectsFilterActionTypes,
  LoadOptions,
  LoadOptionsSuccess,
  LoadOptionsFailure,
  SaveOptions
} from './projects-filter.actions';
import { ProjectConstants } from 'app/entities/projects/project.model';

const { UNASSIGNED_PROJECT_ID } = ProjectConstants;

@Injectable()
export class ProjectsFilterEffects {
  constructor(
    private actions$: Actions,
    private projectsFilter: ProjectsFilterService
  ) { }

  @Effect()
  loadOptions$ = this.actions$.pipe(
    ofType<LoadOptions>(ProjectsFilterActionTypes.LOAD_OPTIONS),
    switchMap(() => {
      return this.projectsFilter.fetchOptions().pipe(
        map((fetched: ProjectsFilterOption[]) => {
          const restored = this.projectsFilter.restoreOptions() || [];
          const merged = mergeOptions(fetched, restored);
          const sorted = sortOptions(merged);
          return new LoadOptionsSuccess(sorted);
        }),
        catchError((error: HttpErrorResponse) => observableOf(new LoadOptionsFailure(error))));
    }));

  @Effect({ dispatch: false })
  saveOptions$ = this.actions$.pipe(
    ofType<SaveOptions>(ProjectsFilterActionTypes.SAVE_OPTIONS),
    tap(({ payload }) => this.projectsFilter.storeOptions(payload))
  );
}

function mergeOptions(fetched, restored) {
  return fetched.map(fetchedOpt => {
    const restoredOpt = find(['value', fetchedOpt.value], restored);
    return restoredOpt ? { ...fetchedOpt, checked: restoredOpt.checked } : fetchedOpt;
  });
}

function sortOptions(options) {
  const sorted = options
    .filter(o => o.value !== UNASSIGNED_PROJECT_ID)
    .sort((a, b) => {
      const opts = { numeric: true, sensitivity: 'base' };
      return a.label.localeCompare(b.label, undefined, opts)
        || a.label.localeCompare(b.label, undefined, { numeric: true });
    });

  const unassignedProject = find(['value', UNASSIGNED_PROJECT_ID], options);
  if (unassignedProject) {
    sorted.push(unassignedProject);
  }

  return sorted;
}
