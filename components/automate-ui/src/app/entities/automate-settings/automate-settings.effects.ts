import { map, mergeMap, catchError } from 'rxjs/operators';
import { forkJoin, of } from 'rxjs';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';

import {
  AutomateSettingsActionTypes,
  ConfigureSettings,
  ConfigureSettingsSuccess,
  ConfigureSettingsFailure,
  GetSettings,
  GetSettingsFailure,
  GetSettingsSuccess
} from './automate-settings.actions';
import { AutomateSettingsRequests } from './automate-settings.requests';

@Injectable()
export class AutomateSettingsEffects {
  constructor(
    private actions$: Actions,
    private requests: AutomateSettingsRequests
  ) {}

  @Effect()
  fetchSettings$ = this.actions$.pipe(
      ofType(AutomateSettingsActionTypes.GET_SETTINGS),
      mergeMap((action: GetSettings) => this.requests.fetchJobSchedulerStatus(action.payload)),
      map(jobSchedulerStatus => new GetSettingsSuccess({ jobSchedulerStatus })),
      catchError((error) => of(new GetSettingsFailure(error)))
  );

  @Effect()
  configureSettings$ = this.actions$.pipe(
    ofType(AutomateSettingsActionTypes.CONFIGURE_SETTINGS),
    mergeMap((action: ConfigureSettings) => {
      const jobsRequests = [];
      action.payload.jobs.forEach((job) => {
        jobsRequests.push(this.requests.configureIngestJob(job));
      });
      return forkJoin(jobsRequests).pipe(
        map((_resp) => new ConfigureSettingsSuccess({})),
        catchError((error) => of(new ConfigureSettingsFailure(error))));
    }));

  @Effect()
  configureSettingsSuccess$ = this.actions$.pipe(
    ofType(AutomateSettingsActionTypes.CONFIGURE_SETTINGS_SUCCESS),
    map((_action) => new GetSettings({})));
}
