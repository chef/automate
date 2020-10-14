import { merge } from 'lodash/fp';

import { NodeCredentialListActions, NodeCredentialListActionTypes } from './node-credential-list.actions';

export enum NodeCredentialOrder {
  asc = 'asc',
  desc = 'desc',
  none = 'none'
}

export interface NodeCredentialFilter {
  key: string;
  values: string[];
}

export interface SortParams {
  page: number;
  per_page: number;
  sort?: string;
  order?: string;
  filters: NodeCredentialFilter[];
}

export interface NodeCredentialListState {
  page: number;
  per_page: number;
  sort: string;
  order: NodeCredentialOrder;
  filters: NodeCredentialFilter[];
}

export const NodeCredentialListInitialState: NodeCredentialListState = {
  page: 1,
  per_page: 100,
  sort: 'name',
  order: NodeCredentialOrder.none,
  filters: [
    {key: 'type', values: ['ssh', 'winrm', 'sudo']}
  ]
};

export function nodeCredentialListReducer(
  state: NodeCredentialListState = NodeCredentialListInitialState,
  action: NodeCredentialListActions): NodeCredentialListState {
  switch (action.type) {

    case NodeCredentialListActionTypes.SORT_NODECREDENTIAL:
      return merge(state, action.payload);

    default:
      return state;
  }

}
