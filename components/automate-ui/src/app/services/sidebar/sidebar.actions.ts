import { Action } from '@ngrx/store';

export interface SidebarAction extends Action {
  payload: any;
}

export const GET_ORGS = 'GET::ORGS';
export const getOrgs =
  (payload = {}): SidebarAction => ({ type: GET_ORGS, payload });

export const GET_ORGS_SUCCESS = 'GET::ORGS::SUCCESS';
export const getOrgsSuccess = (payload): SidebarAction =>
  ({ type: GET_ORGS_SUCCESS, payload });

export const GET_ORGS_ERROR = 'GET::ORGS::ERROR';

export const GET_CHEF_SERVERS = 'GET::CHEF::SERVERS';
export const getChefServers =
  (payload = {}): SidebarAction => ({ type: GET_CHEF_SERVERS, payload });

export const GET_CHEF_SERVERS_SUCCESS = 'GET::CHEF::SERVERS::SUCCESS';
export const getChefServersSuccess = (payload): SidebarAction =>
  ({ type: GET_CHEF_SERVERS_SUCCESS, payload });

export const GET_CHEF_SERVERS_ERROR = 'GET::CHEF::SERVERS::ERROR';

export const SET_SELECTED_SIDEBAR_ORGS = 'SET::SELECTED::SIDEBAR::ORGS';
export const setSelectedSidebarOrgs = (orgs: string[]): SidebarAction =>
  ({ type: SET_SELECTED_SIDEBAR_ORGS, payload: orgs });

export const SET_SELECTED_SIDEBAR_CHEF_SERVERS = 'SET::SELECTED::SIDEBAR::CHEF::SERVERS';
export const setSelectedSidebarChefServers = (chefServers: string[]): SidebarAction =>
  ({ type: SET_SELECTED_SIDEBAR_CHEF_SERVERS, payload: chefServers });
