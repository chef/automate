import { createSelector, createFeatureSelector } from '@ngrx/store';
import { InfraNodeEntityState, nodeEntityAdapter } from './infra-nodes.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const infraNodeState = createFeatureSelector<InfraNodeEntityState>('infraNodes');

export const {
  selectAll: allInfraNodes,
  selectEntities: nodeEntities
} = nodeEntityAdapter.getSelectors(infraNodeState);

export const getAllStatus = createSelector(
  infraNodeState,
  (state) => state.getAllStatus
);

export const infraNodeStatus = createSelector(
  infraNodeState,
  (state) => state.nodesStatus
);

export const getStatus = createSelector(
  infraNodeState,
  (state) => state.getStatus
);

export const updateStatus = createSelector(
  infraNodeState,
  (state) => state.updateStatus
);

export const updateEnvStatus = createSelector(
  infraNodeState,
  (state) => state.updateEnvStatus
);

export const updateTagsStatus = createSelector(
  infraNodeState,
  (state) => state.updateTagsStatus
);

export const nodeList = createSelector(
  infraNodeState,
  (state) => state.nodeList
);

export const nodeTags = createSelector(
  infraNodeState,
  (state) => state.nodeTags
);

export const nodeEnvironment = createSelector(
  infraNodeState,
  (state) => state.nodeEnvironment
);

export const deleteStatus = createSelector(
  infraNodeState,
  (state) => state.deleteStatus
);

export const infraNodeFromRoute = createSelector(
  nodeEntities,
  routeParams,
  (state, { name }) => find({ name }, state)
);
