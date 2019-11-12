import { createSelector, createFeatureSelector } from '@ngrx/store';

import { RouterReducerState } from './ngrx.reducers';

export const routerReducerState = createFeatureSelector<RouterReducerState>('router');

export const routeParams = createSelector(
  routerReducerState,
  ({state}) => state.params
);

export const previousRoute = createSelector(
  routerReducerState,
  (state) => state.previousRoute
);

export const routeURL = createSelector(
  routerReducerState,
  ({state}) => state.url
);

export const routeState = createSelector(
  routerReducerState,
  ({state}) => state
);
