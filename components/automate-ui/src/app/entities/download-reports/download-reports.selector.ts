import { createSelector, createFeatureSelector } from '@ngrx/store';
import { DownloadReportsEntityState } from './download-reports.reducer';

export const downloadReportsState = createFeatureSelector<DownloadReportsEntityState>('downloadReports');

export const downloadNotificationList = createSelector(
  downloadReportsState,
  (downloadReports) => downloadReports.list.notificationItems
);
