import { createSelector, createFeatureSelector } from '@ngrx/store';

import { LayoutEntityState } from './layout.reducer';

export const layoutState = createFeatureSelector<LayoutEntityState>('layout');

export const sidebar = createSelector(
  layoutState,
  (layout) => {
    return layout.sidebars[layout.sidebars.active];
  }
);

export const showPageLoading = createSelector(
    layoutState,
    (layout) => layout.showPageLoading
  );
