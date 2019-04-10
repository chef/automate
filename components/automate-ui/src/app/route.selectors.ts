import { createSelector, createFeatureSelector } from '@ngrx/store';

import { RouterReducerState } from './ngrx.reducers';

export const routerState = createFeatureSelector<RouterReducerState>('router');

export const routeParams = createSelector(
  routerState,
  ({state}) => state.params
);

export const previousRoute = createSelector(
  routerState,
  (state) => state.previousRoute
);

export const routeURL = createSelector(
  routerState,
  ({state}) => state.url
);
