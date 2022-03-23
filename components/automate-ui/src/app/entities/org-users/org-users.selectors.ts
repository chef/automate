import { createSelector, createFeatureSelector } from '@ngrx/store';
import { OrgUserEntityState, orgUserEntityAdapter } from './org-users.reducer';

export const orgUserState = createFeatureSelector<OrgUserEntityState>('OrgUsers');
export const {
  selectAll: allUsers,
  selectEntities: userEntities
} = orgUserEntityAdapter.getSelectors(orgUserState);

export const getAllStatus = createSelector(
  orgUserState,
  (state) => state.getAllStatus
);

export const orgUserList = createSelector(
  orgUserState,
  (state) => state.orgUserList
);
