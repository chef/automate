import { createSelector, createFeatureSelector } from '@ngrx/store';
import { CookbookVersionsEntityState, cookbookVersionsEntityAdapter } from './cookbook-versions.reducer';
import { routeParams } from 'app/route.selectors';

export const cookbookVersionsState = createFeatureSelector<CookbookVersionsEntityState>('cookbookVersions');

export const {
  selectAll: allCookbookVersions,
  selectEntities: cookbookVersionsEntities
} = cookbookVersionsEntityAdapter.getSelectors(cookbookVersionsState);

export const getStatus = createSelector(
  cookbookVersionsState,
  (state) => state.getStatus
);

export const cookbookVersionsFromRoute = createSelector(
  cookbookVersionsEntities,
  routeParams,
  (state, {'cookbook-name': cookbook_name}) => state[cookbook_name]
);
