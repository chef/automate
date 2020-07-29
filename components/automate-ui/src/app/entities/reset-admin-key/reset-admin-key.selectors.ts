import { createSelector, createFeatureSelector } from '@ngrx/store';

import { AdminKeyEntityState, adminKeyEntityAdapter } from './reset-admin-key.reducer';

export const adminKeyState = createFeatureSelector<AdminKeyEntityState>('adminKey');

export const {
  selectAll: allAdminKeys,
  selectEntities: adminKeyEntities
} = adminKeyEntityAdapter.getSelectors(adminKeyState);

export const updateStatus = createSelector(
  adminKeyState,
  (state) => state.updateStatus
);
