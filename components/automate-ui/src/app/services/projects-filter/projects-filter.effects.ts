import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { of as observableOf } from 'rxjs';
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
          const loaded = fetched.map(fetchedOpt => {
            const restoredOpt = restored.filter(opt => opt.value === fetchedOpt.value)[0];
            return restoredOpt ? { ...fetchedOpt, checked: restoredOpt.checked } : fetchedOpt;
          });
          return new LoadOptionsSuccess(loaded);
        }),
        catchError((error: HttpErrorResponse) => observableOf(new LoadOptionsFailure(error))));
    }));

  @Effect({ dispatch: false })
  saveOptions$ = this.actions$.pipe(
    ofType<SaveOptions>(ProjectsFilterActionTypes.SAVE_OPTIONS),
    tap(({ payload }) => this.projectsFilter.storeOptions(payload))
  );
}
