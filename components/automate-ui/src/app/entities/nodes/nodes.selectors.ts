import { createSelector, createFeatureSelector } from '@ngrx/store';
import { omit } from 'lodash';

export interface Nodes {
  nodes: Node[];
}

import { NodesEntityState } from './nodes.reducer';

export const nodesState = createFeatureSelector<NodesEntityState>('nodes');

export const nodesList = createSelector(nodesState, state => state.nodesList);

export const nodesListItems = createSelector(nodesState, state => state.nodesList.items);

export const nodesStatus = createSelector(
    nodesState,
    (state) => state.status
  );

export const nodesListParams = createSelector(nodesList, list => omit(list, 'items', 'total'));
