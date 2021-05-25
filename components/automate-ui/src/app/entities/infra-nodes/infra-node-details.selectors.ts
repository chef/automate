import { createSelector, createFeatureSelector } from '@ngrx/store';
import { InfraNodeDetailsEntityState, infraNodeDetailsEntityAdapter } from './infra-node-details.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const infraNodeState =
  createFeatureSelector<InfraNodeDetailsEntityState>('infraNodeDetails');

export const {
  selectAll: allInfraNodes,
  selectEntities: nodeEntities
} = infraNodeDetailsEntityAdapter.getSelectors(infraNodeState);

export const infraNodeStatus = createSelector(
  infraNodeState,
  (state) => state.nodeStatus
);

export const getStatus = createSelector(
  infraNodeState,
  (state) => state.getStatus
);

export const infraNodeFromRoute = createSelector(
  nodeEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
