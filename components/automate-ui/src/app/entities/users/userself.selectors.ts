import { createSelector, createFeatureSelector } from '@ngrx/store';

import { UserSelfEntityState } from './userself.reducer';

export const userSelfState = createFeatureSelector<UserSelfEntityState>('userSelf');

export const getStatus = createSelector(
  userSelfState,
  (state) => state.getStatus
);

export const userSelf = createSelector(
  userSelfState,
  (state) => state.userSelf
);

export const userSelfId = createSelector(
  userSelfState,
  (state) => state.userSelfId
);

export const updateStatus = createSelector(
  userSelfState,
  (state) => state.updateStatus
);
