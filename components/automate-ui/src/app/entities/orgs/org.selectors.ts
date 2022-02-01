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
  (state, { 'org-id': org_id }) => state[org_id]
);

export const uploadStatus = createSelector(
  orgState,
  (state) => state.uploadStatus
);

export const uploadDetails = createSelector(
  orgState,
  (state) => state.uploadDetails
);
<<<<<<< HEAD

export const cancelStatus = createSelector(
  orgState,
  (state) => state.cancelStatus
);

export const previewStatus = createSelector(
  orgState,
  (state) => state.previewStatus
);

export const previewData = createSelector(
  orgState,
  (state) => state.previewData
);

export const confirmPreviewStatus = createSelector(
  orgState,
  (state) => state.confirmPreviewStatus
);
=======
>>>>>>> d5e176b0b (Stalwart 32 upload slider functionality (#6654))
