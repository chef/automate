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

export const removeUsersStatus = createSelector(
  teamState,
  (state) => state.removeUsersStatus
);

// v1 teams are identified by their guid,
// so we use this selector to extract the correct team
// using what the router refers to as the 'id' (a generic param name)
// (it is actually the team's guid field)
// this is not intuitive, but we've isolated the complexity to the
// getTeam logic
export const v1TeamFromRoute = createSelector(
  teamEntities,
  routeParams,
  (state, { id }) => find({ 'guid': id }, state)
);

// we're identifying teams by the Id in the state
// consistent with how v2 teams will work
// so no need to match on another parameter like above
export const v2TeamFromRoute = createSelector(
  teamEntities,
  routeParams,
  (state, { id }) => find({ id }, state)
);

export const teamUsers = createSelector(
  teamState,
  (state) => state.userIDs
);
