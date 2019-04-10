import map from 'lodash/fp/map';
import pipe from 'lodash/fp/pipe';
import set from 'lodash/fp/set';

import { GET_DOCS,
         GET_DOCS_FAILED,
         GET_DOCS_SUCCESS
       } from './doc.actions';
import { DocEntity } from './doc.entity';
import { IndexedEntities,
         indexer
       } from '../entities';

const docIndexer = indexer(DocEntity.create);

export enum Status {
  notLoaded,
  loading,
  loadSuccess,
  loadFailure
}

export interface DocEntityState {
  readonly byId: IndexedEntities<DocEntity>;
  readonly allIds: string[];
  readonly status: Status;
}

const initialState = {
  byId: {},
  allIds: [],
  status: Status.notLoaded
};

export function docReducer(state: DocEntityState = initialState, action): DocEntityState {
  switch (action.type) {
  case GET_DOCS:
    return set('status', Status.loading, state);
  case GET_DOCS_FAILED:
    console.warn('FAILED GETTING DOCS');
    return set('status', Status.loadFailure, state);
  case GET_DOCS_SUCCESS:
    const docs = docIndexer(action.payload.children);
    return pipe(
      set('status', Status.loadSuccess),
      set('byId', docs),
      set('allIds', map('id', docs))
    )(state) as DocEntityState;
  default:
    return state;
  }
}
