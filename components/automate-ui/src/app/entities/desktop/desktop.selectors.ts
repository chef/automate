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

export const unknownDesktopDurationCounts = createSelector(
  desktopState,
  (state) => state.unknownDesktopDurationCounts
);

export const desktops = createSelector(
  desktopState,
  (state) => state.desktops
);

export const desktopsTotal = createSelector(
  desktopState,
  (state) => state.desktopsTotal
);

export const desktopsCurrentPage = createSelector(
  desktopState,
  (state) => state.getDesktopsFilter.currentPage
);

export const desktopsPageSize = createSelector(
  desktopState,
  (state) => state.getDesktopsFilter.pageSize
);

export const desktopsFilterTerms = createSelector(
  desktopState,
  (state) => state.getDesktopsFilter.terms
);
