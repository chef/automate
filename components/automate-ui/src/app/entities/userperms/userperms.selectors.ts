import { createSelector, createFeatureSelector } from '@ngrx/store';
import { map, get } from 'lodash/fp';
import { PermEntityState } from './userperms.reducer';

const NGRX_REDUCERS_KEY = 'userperms';

export const userpermsState = createFeatureSelector<PermEntityState>(NGRX_REDUCERS_KEY);

export const permList = createSelector(
  userpermsState,
  (state) => map((id) => get(['byId', id], state),
                 get('allIds', state))
);

export const allPerms = createSelector(
  userpermsState,
  get('byId')
);

export const loadStatus = createSelector(
  userpermsState,
  get('status')
);
