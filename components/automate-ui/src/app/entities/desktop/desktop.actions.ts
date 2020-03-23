import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { DailyCheckInCountCollection } from './desktop.model';

export enum DesktopActionTypes {
  GET_DAILY_CHECK_IN_TIME_SERIES         = 'DESKTOP::GET_DAILY_CHECK_IN_TIME_SERIES',
  GET_DAILY_CHECK_IN_TIME_SERIES_SUCCESS = 'DESKTOP::GET_DAILY_CHECK_IN_TIME_SERIES::SUCCESS',
  GET_DAILY_CHECK_IN_TIME_SERIES_FAILURE = 'DESKTOP::GET_DAILY_CHECK_IN_TIME_SERIES::FAILURE'
}

export class GetDailyCheckInTimeSeries implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES;
}

export class GetDailyCheckInTimeSeriesSuccess implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_SUCCESS;
  constructor(public payload: DailyCheckInCountCollection) { }
}

export class GetDailyCheckInTimeSeriesFailure implements Action {
  readonly type = DesktopActionTypes.GET_DAILY_CHECK_IN_TIME_SERIES_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type DesktopActions =
  | GetDailyCheckInTimeSeries
  | GetDailyCheckInTimeSeriesSuccess
  | GetDailyCheckInTimeSeriesFailure;
