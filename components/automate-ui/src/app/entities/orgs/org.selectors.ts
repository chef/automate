import { createSelector, createFeatureSelector } from '@ngrx/store';

import { routeParams } from 'app/route.selectors';
import { OrgEntityState, orgEntityAdapter } from './org.reducer';

export const orgState = createFeatureSelector<OrgEntityState>('orgs');

export const {
  selectAll: allOrgs,
  selectEntities: orgEntities
} = orgEntityAdapter.getSelectors(orgState);

export const getAllStatus = createSelector(
  orgState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
  orgState,
  (state) => state.getStatus
);

export const createStatus = createSelector(
  orgState,
  (state) => state.createStatus
);

export const createError = createSelector(
  orgState,
  (state) => state.createError
);

export const updateStatus = createSelector(
  orgState,
  (state) => state.updateStatus
);

export const deleteStatus = createSelector(
  orgState,
  (state) => state.deleteStatus
);

export const orgFromRoute = createSelector(
  orgEntities,
  routeParams,
  (state, { orgid }) => state[orgid]
);
