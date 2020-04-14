import { createSelector, createFeatureSelector } from '@ngrx/store';
import { CookbookVersionsEntityState, cookbookVersionsEntityAdapter } from './cookbookversions.reducer';

export const cookbookVersionsState = createFeatureSelector<CookbookVersionsEntityState>('cookbookVersions');

export const {
  selectAll: allCookbookVersions,
  selectEntities: cookbookVersionsEntities
} = cookbookVersionsEntityAdapter.getSelectors(cookbookVersionsState);

export const getStatus = createSelector(
  cookbookVersionsState,
  (state) => state.getStatus
);
