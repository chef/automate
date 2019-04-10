import { createSelector, createFeatureSelector } from '@ngrx/store';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

import { RoleEntityState, roleEntityAdapter } from './role.reducer';

export const roleState = createFeatureSelector<RoleEntityState>('roles');

export const {
  selectAll: allRoles,
  selectEntities: roleEntities
} = roleEntityAdapter.getSelectors(roleState);

export const getAllStatus = createSelector(
  roleState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
  roleState,
  (state) => state.getStatus
);

export const roleFromRoute = createSelector(
  roleEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);

