import { Action } from '@ngrx/store';

import { Profile } from './profile.model';

export enum ProfileActionTypes {
  PROFILES_SEARCH = 'PROFILE::SEARCH',
  PROFILES_SEARCH_SUCCESS = 'PROFILE::SEARCH::SUCCESS',
  PROFILES_SEARCH_FAILURE = 'PROFILE::SEARCH::FAILURE'
}

export interface ProfilesSearchPayload {
  owner?: string;
  filters?: any[];
  page?: number;
  per_page?: number;
  sort?: string;
  order?: string;
}
export class ProfilesSearch implements Action {
  readonly type = ProfileActionTypes.PROFILES_SEARCH;
  constructor(public payload: ProfilesSearchPayload) {}
}

export interface ProfilesSearchSuccessPayload {
  profiles: Profile[];
  total: number;
}
export class ProfilesSearchSuccess implements Action {
  readonly type = ProfileActionTypes.PROFILES_SEARCH_SUCCESS;
  constructor(public payload: ProfilesSearchSuccessPayload) {}
}

export class ProfilesSearchFailure implements Action {
  readonly type = ProfileActionTypes.PROFILES_SEARCH_FAILURE;
  constructor(public payload: {}) {}
}

export type ProfileActions =
  | ProfilesSearch
  | ProfilesSearchSuccess
  | ProfilesSearchFailure;
