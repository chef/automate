import { createSelector, createFeatureSelector } from '@ngrx/store';
import { InfraNodeEntityState, nodeEntityAdapter } from './infra-nodes.reducer';

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
  infraNodeState,
  (state) => state.node
);

export const updateAttributesStatus = createSelector(
  infraNodeState,
  (state) => state.updateAttributesStatus
);

export const getAllNodesStatus = createSelector(
  infraNodeState,
  (state) => state.getAllNodesStatus
);

export const policyGroupNodeList = createSelector(
  infraNodeState,
  (state) => state.policyGroupNodeList
);
