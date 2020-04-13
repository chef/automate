import { createSelector, createFeatureSelector } from '@ngrx/store';
import {
  CookbookDetailsEntityState,
  cookbookDetailsEntityAdapter
} from './cookbookdetails.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const cookbookDetailsState = createFeatureSelector<CookbookDetailsEntityState>('cookbookDetails');

export const {
  selectAll: allCookbookDetails,
  selectEntities: cookbookDetailsEntities
} = cookbookDetailsEntityAdapter.getSelectors(cookbookDetailsState);

export const getStatus = createSelector(
  cookbookDetailsState,
  (state) => state.getStatus
);

export const cookbookDetailsFromRoute = createSelector(
  cookbookDetailsEntities,
  routeParams,
  (state, { cookbook_name }) => find({ cookbook_name }, state)
);
