import { createSelector, createFeatureSelector } from '@ngrx/store';
import { NodeRunlistEntityState, nodeRunlistEntityAdapter } from './nodeRunlists.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const nodeRunlistState =
  createFeatureSelector<NodeRunlistEntityState>('nodeRunlist');

export const {
  selectAll: allNodeRunlist,
  selectEntities: nodeRunlistEntities
} = nodeRunlistEntityAdapter.getSelectors(nodeRunlistState);

export const nodeRunlistStatus = createSelector(
  nodeRunlistState,
  (state) => state.nodeRunlistsStatus
);

export const getAllStatus = createSelector(
  nodeRunlistState,
  (state) => state.getAllStatus
);

export const nodeRunlistFromRoute = createSelector(
  nodeRunlistEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);
