import { map, mergeMap, catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';

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

  fetchSettings$ = createEffect(() =>
    this.actions$.pipe(
      ofType(AutomateSettingsActionTypes.GET_SETTINGS),
      mergeMap((action: GetSettings) => this.requests.fetchJobSchedulerStatus(action.payload)),
      map(jobSchedulerStatus => new GetSettingsSuccess({ jobSchedulerStatus })),
      catchError((error) => of(new GetSettingsFailure(error)))
    ));

  configureSettings$ = createEffect(() =>
    this.actions$.pipe(
      ofType(AutomateSettingsActionTypes.CONFIGURE_SETTINGS),
      mergeMap(
        (action: ConfigureSettings) => this.requests.configureIngestJobs(action.payload.jobs)),
      map((_resp) => (new ConfigureSettingsSuccess({}))),
      catchError((error) => of(new ConfigureSettingsFailure(error))
    )));

}
