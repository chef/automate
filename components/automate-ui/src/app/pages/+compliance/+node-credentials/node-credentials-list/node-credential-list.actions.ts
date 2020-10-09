import { Action } from '@ngrx/store';

import { NodeCredentialOrder } from './node-credential-list.reducer';

export enum NodeCredentialListActionTypes {
  SORT_NODECREDENTIAL = 'NODECREDENTIAL_LIST::SORT'
}

export class SortNodeCredentialList implements Action {
  readonly type = NodeCredentialListActionTypes.SORT_NODECREDENTIAL;

  constructor(public payload: { order: NodeCredentialOrder, sort: string }) {}
}

export type NodeCredentialListActions = SortNodeCredentialList;
