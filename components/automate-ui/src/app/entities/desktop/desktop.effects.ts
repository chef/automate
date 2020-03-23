import { catchError, mergeMap, map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';

import {
  // GetDailyCheckInTimeSeries,
  GetDailyCheckInTimeSeriesSuccess,
  GetDailyCheckInTimeSeriesFailure,
  DesktopActionTypes
} from './desktop.actions';
import { DesktopRequests } from './desktop.requests';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

@Injectable()
export class DesktopEffects {
  constructor(
    private actions$: Actions,
    private requests: DesktopRequests
  ) { }

  @Effect()
  getDailyCheckInTimeSeries$ = this.actions$.pipe(
    ofType(DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES),
    mergeMap(() => this.requests.getDailyCheckInCountCollection()),
    map(dailyCheckInCountCollection =>
      new GetDailyCheckInTimeSeriesSuccess(dailyCheckInCountCollection)),
    catchError((error) => of(new GetDailyCheckInTimeSeriesFailure(error)))
  );

  @Effect()
  getDailyCheckInTimeSeriesFailure$ = this.actions$.pipe(
    ofType(DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_FAILURE),
    map(({ payload: { error } }: GetDailyCheckInTimeSeriesFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get time series: ${msg || error}`
      });
    }));
}
