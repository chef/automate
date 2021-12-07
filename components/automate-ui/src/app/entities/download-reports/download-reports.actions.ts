import { Action } from '@ngrx/store';

export enum DownloadReportsActionTypes {
  ADD = 'DOWNLOAD_REPORTS::ADD',
  CLEAR = 'DOWNLOAD_REPORTS::CLEAR'
}

export class AckDownloadReports implements Action {
  readonly type = DownloadReportsActionTypes.ADD;
  constructor(public payload: string) {}
}

export class ClearNotificationReport implements Action {
  readonly type = DownloadReportsActionTypes.CLEAR;
  constructor(public payload: string) {}
}

export type DownloadReportsActions =
  | AckDownloadReports
  | ClearNotificationReport;
