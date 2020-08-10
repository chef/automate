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

export const getSelected = createSelector(
  desktopState,
  (state) => state.selected
);

export const getSelectedDaysAgo = createSelector(
  desktopState,
  (state) => state.selected.daysAgo
);

export const getSelectedDesktop = createSelector(
  desktopState,
  (state) => state.selected.desktop
);

export const getSelectedNodeRun = createSelector(
  desktopState,
  (state) => state.selected.nodeRun
);

export const getDailyNodeRuns = createSelector(
  desktopState,
  (state) => state.dailyNodeRuns
);

export const topErrorsCollection = createSelector(
  desktopState,
  (state) => state.topErrorCollection
);

export const unknownDesktopDurationCounts = createSelector(
  desktopState,
  (state) => state.unknownDesktopDurationCounts
);

export const nodeMetadataCounts = createSelector(
  desktopState,
  (state) => state.nodeMetadataCounts
);

export const desktopListTitle = createSelector(
  desktopState,
  (state) => state.desktopListTitle
);

export const desktopListColumns = createSelector(
  desktopState,
  (state) => state.desktopListColumns
);

export const desktopListColumnsSaveAsDefault = createSelector(
  desktopState,
  (state) => state.desktopListColumnsSaveAsDefault
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
