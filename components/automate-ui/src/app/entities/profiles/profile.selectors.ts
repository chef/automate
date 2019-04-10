import { createSelector, createFeatureSelector } from '@ngrx/store';

import { ProfileEntityState, profileEntityAdapter } from './profile.reducer';


export const profileState = createFeatureSelector<ProfileEntityState>('profiles');

export const {
  selectIds: profileIds,
  selectEntities: profileEntities,
  selectAll: allProfiles,
  selectTotal: totalProfiles
} = profileEntityAdapter.getSelectors(profileState);

export const profileStatus = createSelector(
  profileState,
  (state) => state.status
);
