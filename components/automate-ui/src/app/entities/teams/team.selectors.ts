import { createSelector, createFeatureSelector } from '@ngrx/store';

import { TeamEntityState, teamEntityAdapter } from './team.reducer';
import { routeParams } from 'app/route.selectors';
import { find } from 'lodash/fp';

export const teamState = createFeatureSelector<TeamEntityState>('teams');

export const {
  selectAll: allTeams,
  selectEntities: teamEntities
} = teamEntityAdapter.getSelectors(teamState);


export const getAllStatus = createSelector(
  teamState,
  (state) => state.getAllStatus
);

export const getStatus = createSelector(
  teamState,
  (state) => state.getStatus
);

export const getUsersStatus = createSelector(
  teamState,
  (state) => state.getUsersStatus
);

export const createStatus = createSelector(
  teamState,
  (state) => state.createStatus
);

export const createError = createSelector(
  teamState,
  (state) => state.createError
);

export const updateStatus = createSelector(
  teamState,
  (state) => state.updateStatus
);

export const deleteStatus = createSelector(
  teamState,
  (state) => state.deleteStatus
);

export const addUsersStatus = createSelector(
  teamState,
  (state) => state.addUsersStatus
);

export const addUsersStatusError = createSelector(
  teamState,
  (state) => state.addUsersStatusError
);


export const removeUsersStatus = createSelector(
  teamState,
  (state) => state.removeUsersStatus
);

export const teamFromRoute = createSelector(
  teamEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);

export const teamUsers = createSelector(
  teamState,
  (state) => state.userIDs
);
