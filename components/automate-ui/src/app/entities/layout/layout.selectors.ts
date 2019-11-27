import { createSelector, createFeatureSelector } from '@ngrx/store';

import { LayoutEntityState } from './layout.reducer';

export const layoutState = createFeatureSelector<LayoutEntityState>('layout');

export const sidebarMenuGroups = createSelector(
    layoutState,
    (layout) => layout.menuGroups
  );

  export const showPageLoading = createSelector(
    layoutState,
    (layout) => layout.showPageLoading
  );
