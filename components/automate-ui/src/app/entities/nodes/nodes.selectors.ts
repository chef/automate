import { createSelector } from '@ngrx/store';
import { omit } from 'lodash';

export interface Nodes {
    nodes: Node[];
}
export const nodesState = state => state.nodes;

export const nodesList = createSelector(nodesState, state => state.nodesList);

export const nodesListItems = createSelector(nodesList, list => list.items);

export const nodesStatus = createSelector(
    nodesState,
    (state) => state.status
  );

export const nodesListParams = createSelector(nodesList, list => omit(list, 'items', 'total'));
