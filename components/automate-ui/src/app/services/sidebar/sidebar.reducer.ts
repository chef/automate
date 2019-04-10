import { LoadingStatus } from '../../types/types';
import {
  GET_ORGS,
  GET_ORGS_SUCCESS,
  GET_ORGS_ERROR,
  GET_CHEF_SERVERS,
  GET_CHEF_SERVERS_SUCCESS,
  GET_CHEF_SERVERS_ERROR,
  SET_SELECTED_SIDEBAR_ORGS,
  SET_SELECTED_SIDEBAR_CHEF_SERVERS,
  SidebarAction } from './sidebar.actions';

import {
 set,
 pipe
} from 'lodash/fp';

export interface SidebarState {
  selectedOrgs: string[];
  selectedChefServers: string[];
  allOrgs: string[];
  allChefServers: string[];
  allOrgsLoadingStatus: LoadingStatus;
  allChefServersLoadingStatus: LoadingStatus;
}

export const initialState: SidebarState = {
  selectedOrgs: [],
  selectedChefServers: [],
  allOrgs: [],
  allChefServers: [],
  allOrgsLoadingStatus: LoadingStatus.notLoaded,
  allChefServersLoadingStatus: LoadingStatus.notLoaded
};

export function sidebarReducer(
  state: SidebarState = initialState,
  action: SidebarAction): SidebarState {

  switch (action.type) {

    case GET_ORGS: {
      return set('allOrgsLoadingStatus', LoadingStatus.loading, state) as SidebarState;
    }

    case GET_ORGS_SUCCESS: {
      return pipe(
        set('allOrgs', action.payload),
        set('allOrgsLoadingStatus', LoadingStatus.loadingSuccess)
      )(state) as SidebarState;
    }

    case GET_ORGS_ERROR: {
      return set('allOrgsLoadingStatus', LoadingStatus.loadingFailure, state) as SidebarState;
    }

    case GET_CHEF_SERVERS: {
      return set('allChefServersLoadingStatus', LoadingStatus.loading, state) as SidebarState;
    }

    case GET_CHEF_SERVERS_SUCCESS: {
      return pipe(
        set('allChefServers', action.payload),
        set('allChefServersLoadingStatus', LoadingStatus.loadingSuccess)
      )(state) as SidebarState;
    }

    case GET_CHEF_SERVERS_ERROR: {
      return set(
        'allChefServersLoadingStatus', LoadingStatus.loadingFailure, state
      ) as SidebarState;
    }

    case SET_SELECTED_SIDEBAR_ORGS: {
      return set('selectedOrgs', action.payload, state) as SidebarState;
    }

    case SET_SELECTED_SIDEBAR_CHEF_SERVERS: {
      return set('selectedChefServers', action.payload, state) as SidebarState;
    }
  }

  return state;
}
