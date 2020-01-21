import { createSelector, createFeatureSelector } from '@ngrx/store';

import { UserEntityState, userEntityAdapter } from './user.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const userState = createFeatureSelector<UserEntityState>('users');

export const {
  selectAll: allUsers,
  selectEntities: userEntities
} = userEntityAdapter.getSelectors(userState);

export const getStatus = createSelector(
  userState,
  (state) => state.getStatus
);

export const updateStatus = createSelector(
  userState,
  (state) => state.updateStatus
);

export const deleteStatus = createSelector(
  userState,
  (state) => state.deleteStatus
);

export const createStatus = createSelector(
  userState,
  (state) => state.createStatus
);

export const createError = createSelector(
  userState,
  (state) => state.createError
);

export const userFromRoute = createSelector(
  userEntities,
  routeParams,
  (state, {id}) => find({id}, state)
);

export const getUserById = (id) => createSelector(
  userEntities,
  (state) => state[id]
);
