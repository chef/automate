import { createSelector, createFeatureSelector } from '@ngrx/store';
import { routeParams } from 'app/route.selectors';
import { CookbookEntityState, cookbookEntityAdapter } from './cookbook.reducer';

export const cookbookState = createFeatureSelector<CookbookEntityState>('cookbooks');

export const {
  selectAll: allCookbooks,
  selectEntities: cookbookEntities
} = cookbookEntityAdapter.getSelectors(cookbookState);

export const getAllStatus = createSelector(
  cookbookState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
  cookbookState,
  (state) => state.getStatus
);

export const cookbookFromRoute = createSelector(
  cookbookEntities,
  routeParams,
  (state, { cookbookid }) => state[cookbookid]
);

