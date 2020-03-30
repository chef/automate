import { createSelector, createFeatureSelector } from '@ngrx/store';

import { DesktopEntityState } from './desktop.reducer';

export const desktopState = createFeatureSelector<DesktopEntityState>('desktops');

export const getDailyCheckInTimeSeriesStatus = createSelector(
  desktopState,
  (state) => state.getDailyCheckInTimeSeriesStatus
);

export const dailyCheckInCountCollection = createSelector(
  desktopState,
  (state) => state.dailyCheckInCountCollection
);

export const getSelectedDaysAgo = createSelector(
  desktopState,
  (state) => state.selectedDaysAgo
);

export const topErrorsCollection = createSelector(
  desktopState,
  (state) => state.topErrorCollection
);
