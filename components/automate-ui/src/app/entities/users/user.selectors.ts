import { createSelector, createFeatureSelector } from '@ngrx/store';

import { UserEntityState, userEntityAdapter } from './user.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const userState = createFeatureSelector<UserEntityState>('users');

export const {
  selectAll: allUsers,
  selectEntities: userEntities
} = userEntityAdapter.getSelectors(userState);

export const userStatus = createSelector(
  userState,
  (state) => state.status
);

export const updateStatus = createSelector(
  userState,
  (state) => state.updateStatus
);

export const userFromRoute = createSelector(
  userEntities,
  routeParams,
  (state, {id}) => find({id}, state)
);
