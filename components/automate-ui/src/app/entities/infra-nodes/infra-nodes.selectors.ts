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
