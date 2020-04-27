import { createSelector, createFeatureSelector } from '@ngrx/store';
import { find } from 'lodash/fp';
import { CookbookEntityState, cookbookEntityAdapter } from './cookbook.reducer';
import { routeParams } from 'app/route.selectors';

export const cookbookState = createFeatureSelector<CookbookEntityState>('cookbooks');

export const {
  selectAll: allCookbooks,
  selectEntities: cookbookEntities
} = cookbookEntityAdapter.getSelectors(cookbookState);

export const getAllStatus = createSelector(
  cookbookState,
  (state) => state.getAllStatus
);

export const cookbookFromRoute = createSelector(
  cookbookEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
