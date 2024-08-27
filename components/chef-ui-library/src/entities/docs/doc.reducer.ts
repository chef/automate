import { createSlice } from '@reduxjs/toolkit';
import map from 'lodash/fp/map';

import { DocEntity } from './doc.entity';
import { IndexedEntities, indexer } from '../entities';

export interface DocEntityState {
  readonly byId: IndexedEntities<DocEntity>;
  readonly allIds: string[];
  readonly status: Status;
}

export enum Status {
  notLoaded,
  loading,
  loadSuccess,
  loadFailure
}

const initialState: DocEntityState = {
  byId: {},
  allIds: [],
  status: Status.notLoaded
};

const docIndexer = indexer(DocEntity.create);

export const docsSlice = createSlice({
  name: 'docs',
  initialState,
  reducers: {
    getDocs: state => {
      state.status = Status.loading
    },
    getDocsFailed: state => {
      console.warn('FAILED GETTING DOCS');
      state.status = Status.loadFailure
    },
    getDocsSuccess: (state, action) => {
      const docs = docIndexer(action.payload.children);
      state.status = Status.loadSuccess;
      state.byId = { ...docs };
      state.allIds = map('id', docs);
    }
  }
})

export const { getDocs } = docsSlice.actions
