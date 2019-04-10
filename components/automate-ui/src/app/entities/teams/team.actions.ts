import { Action } from '@ngrx/store';
import { HttpErrorResponse } from '@angular/common/http';

import { Team } from './team.model';

export enum TeamActionTypes {
  GET_ALL               = 'TEAM::GET_ALL',
  GET_ALL_SUCCESS       = 'TEAM::GET_ALL::SUCCESS',
  GET_ALL_FAILURE       = 'TEAM::GET_ALL::FAILURE',
  GET                   = 'TEAM::GET',
  GET_SUCCESS           = 'TEAM::GET::SUCCESS',
  GET_FAILURE           = 'TEAM::GET::FAILURE',
  GET_USERS             = 'TEAM::GET_USERS',
  GET_USERS_SUCCESS     = 'TEAM::GET_USERS::SUCCESS',
  GET_USERS_FAILURE     = 'TEAM::GET_USERS::FAILURE',
  CREATE                = 'TEAM::CREATE',
  CREATE_SUCCESS        = 'TEAM::CREATE::SUCCESS',
  CREATE_FAILURE        = 'TEAM::CREATE::FAILURE',
  UPDATE                = 'TEAM::UPDATE',
  UPDATE_SUCCESS        = 'TEAM::UPDATE::SUCCESS',
  UPDATE_FAILURE        = 'TEAM::UPDATE::FAILURE',
  DELETE                = 'TEAM::DELETE',
  DELETE_SUCCESS        = 'TEAM::DELETE::SUCCESS',
  DELETE_FAILURE        = 'TEAM::DELETE::FAILURE',
  ADD_USERS             = 'TEAM::ADD_USERS',
  ADD_USERS_SUCCESS     = 'TEAM::ADD_USERS::SUCCESS',
  ADD_USERS_FAILURE     = 'TEAM::ADD_USERS::FAILURE',
  REMOVE_USERS          = 'TEAM::REMOVE_USERS',
  REMOVE_USERS_SUCCESS  = 'TEAM::REMOVE_USERS::SUCCESS',
  REMOVE_USERS_FAILURE  = 'TEAM::REMOVE_USERS::FAILURE'
}

export class GetTeams implements Action {
  readonly type = TeamActionTypes.GET_ALL;
}

export interface GetTeamsSuccessPayload {
  teams: Team[];
}

export interface GetTeamUsersSuccessPayload {
  user_ids: string[];
}

export class GetTeamsSuccess implements Action {
  readonly type = TeamActionTypes.GET_ALL_SUCCESS;

  constructor(public payload: GetTeamsSuccessPayload) { }
}

export class GetTeamsFailure implements Action {
  readonly type = TeamActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetTeam implements Action {
  readonly type = TeamActionTypes.GET;

  constructor(public payload: { id: string }) { }
}

export class GetTeamSuccess implements Action {
  readonly type = TeamActionTypes.GET_SUCCESS;

  constructor(public payload: Team) { }
}

export class GetTeamFailure implements Action {
  readonly type = TeamActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetTeamUsers implements Action {
  readonly type = TeamActionTypes.GET_USERS;

  constructor(public payload: { id: string }) { }
}

export class GetTeamUsersSuccess implements Action {
  readonly type = TeamActionTypes.GET_USERS_SUCCESS;

  constructor(public payload: GetTeamUsersSuccessPayload) { }
}

export class GetTeamUsersFailure implements Action {
  readonly type = TeamActionTypes.GET_USERS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export interface CreateTeamPayload {
  id: string;
  name: string;
  projects: string[];
}

export interface CreateTeamV1Payload {
  name: string;
  description: string;
}

export class CreateTeam implements Action {
  readonly type = TeamActionTypes.CREATE;

  constructor(public payload: CreateTeamPayload) { }
}

export class CreateTeamSuccess implements Action {
  readonly type = TeamActionTypes.CREATE_SUCCESS;

  constructor(public payload: Team) { }
}

export class CreateTeamFailure implements Action {
  readonly type = TeamActionTypes.CREATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class UpdateTeam implements Action {
  readonly type = TeamActionTypes.UPDATE;
  constructor(public payload: Team) { }
}

export class UpdateTeamSuccess implements Action {
  readonly type = TeamActionTypes.UPDATE_SUCCESS;
  constructor(public payload: Team) { }
}

export class UpdateTeamFailure implements Action {
  readonly type = TeamActionTypes.UPDATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class DeleteTeam implements Action {
  readonly type = TeamActionTypes.DELETE;

  constructor(public payload: Team) { }
}

export class DeleteTeamSuccess implements Action {
  readonly type = TeamActionTypes.DELETE_SUCCESS;

  constructor(public payload: Team) { }
}

export class DeleteTeamFailure implements Action {
  readonly type = TeamActionTypes.DELETE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export interface TeamUserMgmtPayload {
  id: string;
  user_ids: string[];
}
export class AddTeamUsers implements Action {
  readonly type = TeamActionTypes.ADD_USERS;

  constructor(public payload: TeamUserMgmtPayload) { }
}

export class AddTeamUsersSuccess implements Action {
  readonly type = TeamActionTypes.ADD_USERS_SUCCESS;

  constructor(public payload: TeamUserMgmtPayload) { }
}

export class AddTeamUsersFailure implements Action {
  readonly type = TeamActionTypes.ADD_USERS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class RemoveTeamUsers implements Action {
  readonly type = TeamActionTypes.REMOVE_USERS;

  constructor(public payload: TeamUserMgmtPayload) { }
}

export class RemoveTeamUsersSuccess implements Action {
  readonly type = TeamActionTypes.REMOVE_USERS_SUCCESS;

  constructor(public payload: TeamUserMgmtPayload) { }
}

export class RemoveTeamUsersFailure implements Action {
  readonly type = TeamActionTypes.REMOVE_USERS_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export type TeamActions =
  | GetTeams
  | GetTeamsSuccess
  | GetTeamsFailure
  | GetTeam
  | GetTeamSuccess
  | GetTeamFailure
  | GetTeamUsers
  | GetTeamUsersSuccess
  | GetTeamUsersFailure
  | CreateTeam
  | CreateTeamSuccess
  | CreateTeamFailure
  | UpdateTeam
  | UpdateTeamSuccess
  | UpdateTeamFailure
  | DeleteTeam
  | DeleteTeamSuccess
  | DeleteTeamFailure
  | AddTeamUsers
  | AddTeamUsersSuccess
  | AddTeamUsersFailure
  | RemoveTeamUsers
  | RemoveTeamUsersSuccess
  | RemoveTeamUsersFailure;
