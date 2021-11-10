import { Action } from '@ngrx/store';

export enum DownloadReportsActionTypes {
  ADD = 'DOWNLOAD_REPORTS::ADD'
}

export class AckDownloadReports implements Action {
  readonly type = DownloadReportsActionTypes.ADD;
  constructor(public payload: string) {}
}

export type DownloadReportsActions =
  | AckDownloadReports;

