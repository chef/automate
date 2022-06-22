import { createSelector, createFeatureSelector } from '@ngrx/store';
import { RunlistEntityState, runlistEntityAdapter } from './runlists.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const runlistState =
  createFeatureSelector<RunlistEntityState>('runlist');

export const {
  selectAll: allRunlist,
  selectEntities: runlistEntities
} = runlistEntityAdapter.getSelectors(runlistState);

export const runlistStatus = createSelector(
  runlistState,
  (state) => state.runlistsStatus
);

export const getAllStatus = createSelector(
  runlistState,
  (state) => state.getAllStatus
);

export const runlistFromRoute = createSelector(
  runlistEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);
