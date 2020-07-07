import { createSelector, createFeatureSelector } from '@ngrx/store';

import { CdsEntityState } from './cds.reducer';

export const cdsState = createFeatureSelector<CdsEntityState>('cds');

export const contentItems = createSelector(
  cdsState,
  (state) => state.contentItems
);

export const contentEnabled = createSelector(
  cdsState,
  (state) => state.isContentEnabled
);
