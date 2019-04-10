import { createSelector } from '@ngrx/store';

export const sidebarState = state => state.sidebar;
export const allOrgs = createSelector(sidebarState, state => state.allOrgs);
export const selectedOrgs = createSelector(sidebarState, state => state.selectedOrgs);
export const allChefServers = createSelector(sidebarState, state => state.allChefServers);
export const selectedChefServers = createSelector(sidebarState,
    state => state.selectedChefServers);
export const allOrgsLoadingStatus = createSelector(sidebarState,
    state => state.allOrgsLoadingStatus);
export const allChefServersLoadingStatus = createSelector(sidebarState,
    state => state.allChefServersLoadingStatus);
