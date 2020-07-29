import { set, pipe } from 'lodash/fp';
import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { assign } from 'lodash';

import { NodesActions, NodesActionTypes } from './nodes.actions';
import { Node } from './nodes.model';
import { EntityStatus } from '../entities';


export interface NodesEntityState extends EntityState<Node> {
  status: EntityStatus;
  nodesList: {
    loading?: boolean,
    items?: Node[],
    total?: number,
    per_page?: number,
    page?: number,
    sort?: string,
    order?: string,
    filters?: any[]
  };
  nodeTotals: {
    all: number,
    unreachable: number,
    reachable: number,
    unknown: number
  };
  nodes: Node[];
}

export const nodesEntityAdapter: EntityAdapter<Node> = createEntityAdapter<Node>();

export const NodesEntityInitialState: NodesEntityState = nodesEntityAdapter.getInitialState({
  status: EntityStatus.notLoaded,
  nodesList: {
    loading: false,
    items: [],
    total: 0,
    per_page: 100,
    page: 1,
    sort: 'last_contact',
    order: 'DESC',
    filters: []
  },
  nodeTotals: {
    all: 0,
    unreachable: 0,
    reachable: 0,
    unknown: 0
  },
  nodes: []
});

export function nodesEntityReducer(state: NodesEntityState = NodesEntityInitialState,
  action: NodesActions): NodesEntityState {

  switch (action.type) {
    case NodesActionTypes.LIST_NODES: {
      const {page, per_page, sort, order, filters} = action.payload;
      set('status', EntityStatus.loading, state);
      const nodesList = assign({}, state.nodesList,
        {page, per_page, sort, order, filters});
      return assign({}, state, {nodesList});
    }

    case NodesActionTypes.LIST_NODES_SUCCESS: {
      const { nodes, total, total_reachable, total_unreachable, total_unknown } = action.payload;
      return pipe(
        set('nodesList.loading', false),
        set('nodesList.items', nodes || []),
        set('nodesList.total', total || 0),
        set('nodeTotals', {
          all: total_reachable + total_unreachable + total_unknown,
          reachable: total_reachable,
          unreachable: total_unreachable,
          unknown: total_unknown
        })
      )(state) as NodesEntityState;
    }

    default:
      return state;

  }
}
