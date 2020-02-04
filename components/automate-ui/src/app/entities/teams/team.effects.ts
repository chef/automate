import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import {
  GetTeam,
  GetTeamSuccess,
  GetTeamFailure,
  GetTeams,
  GetTeamsSuccess,
  GetTeamsSuccessPayload,
  GetTeamsFailure,
  CreateTeam,
  CreateTeamSuccess,
  CreateTeamFailure,
  UpdateTeam,
  UpdateTeamSuccess,
  UpdateTeamFailure,
  DeleteTeam,
  DeleteTeamSuccess,
  DeleteTeamFailure,
  GetTeamUsers,
  GetTeamUsersSuccess,
  GetTeamUsersFailure,
  AddTeamUsers,
  AddTeamUsersSuccess,
  AddTeamUsersFailure,
  RemoveTeamUsers,
  RemoveTeamUsersSuccess,
  RemoveTeamUsersFailure,
  TeamActionTypes
} from './team.actions';
import { TeamRequests, UsersResponse, TeamResponse } from './team.requests';

@Injectable()
export class TeamEffects {
  constructor(
    private actions$: Actions,
    private requests: TeamRequests
  ) { }

  @Effect()
  getTeams$ = this.actions$.pipe(ofType<GetTeams>(TeamActionTypes.GET_ALL),
    mergeMap((_action: GetTeams) =>
      this.requests.getTeams().pipe(
        map((resp: GetTeamsSuccessPayload) => {
          return new GetTeamsSuccess({ teams: resp.teams });
        }),
        catchError((error: HttpErrorResponse) => observableOf(new GetTeamsFailure(error))))
    ));

  @Effect()
  getTeamsFailure$ = this.actions$.pipe(
    ofType<GetTeamsFailure>(TeamActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetTeamsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get teams: ${msg || payload.error}.`
      });
    }));

  @Effect()
  getTeam$ = this.actions$.pipe(ofType<GetTeam>(TeamActionTypes.GET),
    mergeMap(({ payload: { id } }: GetTeam) =>
      this.requests.getTeam(id).pipe(
        map(({ team }: TeamResponse) => new GetTeamSuccess(team)),
        catchError((error: HttpErrorResponse) => observableOf(new GetTeamFailure(error))))
    ));

  @Effect()
  getTeamFailure$ = this.actions$.pipe(
    ofType<GetTeamFailure>(TeamActionTypes.GET_FAILURE),
    map(({ payload }: GetTeamFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get team: ${msg || payload.error}.`
      });
    }));

  @Effect()
  getTeamUsers$ = this.actions$.pipe(ofType<GetTeamUsers>(TeamActionTypes.GET_USERS),
    mergeMap(({ payload: { id } }: GetTeamUsers) =>
      this.requests.getTeamUsers(id).pipe(
        map((resp: UsersResponse) => new GetTeamUsersSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new GetTeamUsersFailure(error))))));

  @Effect()
  getTeamUsersFailure$ = this.actions$.pipe(
    ofType(TeamActionTypes.GET_USERS_FAILURE),
    map(({ payload }: GetTeamFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get users for team: ${msg || payload.error}.`
      });
    }));

  @Effect()
  createTeam$ = this.actions$.pipe(ofType<CreateTeam>(TeamActionTypes.CREATE),
    mergeMap(({ payload }: CreateTeam) =>
      this.requests.createTeam(payload).pipe(
        map(({ team }: TeamResponse) => new CreateTeamSuccess(team)),
        catchError((error) => observableOf(new CreateTeamFailure(error))))
    ));

  @Effect()
  createTeamSuccess$ = this.actions$.pipe(
    ofType<CreateTeamSuccess>(TeamActionTypes.CREATE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Created a new team.'
    })));

  @Effect()
  createTeamFailure$ = this.actions$.pipe(
    ofType<CreateTeamFailure>(TeamActionTypes.CREATE_FAILURE),
    // ID conflict handled in the modal, see team-management.component.ts
    filter(({ payload: { status } }) => status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateTeamFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not create team: ${msg || payload.error}.`
      });
    }));

  @Effect()
  updateTeam$ = this.actions$.pipe(ofType<UpdateTeam>(TeamActionTypes.UPDATE),
    mergeMap(({ payload }: UpdateTeam) =>
      this.requests.updateTeam(payload).pipe(
        map(({ team }: TeamResponse) => new UpdateTeamSuccess(team)),
        catchError((error) => observableOf(new UpdateTeamFailure(error))))
    ));

  @Effect()
  updateTeamSuccess$ = this.actions$.pipe(
    ofType(TeamActionTypes.UPDATE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Updated team.'
    })));

  @Effect()
  updateTeamFailure$ = this.actions$.pipe(
    ofType(TeamActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateTeamFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update team: ${msg || payload.error}.`
      });
    }));

  @Effect()
  deleteTeam$ = this.actions$.pipe(ofType<DeleteTeam>(TeamActionTypes.DELETE),
    mergeMap(({ payload }: DeleteTeam) =>
      this.requests.deleteTeam(payload).pipe(
        map(({ team }: TeamResponse) => new DeleteTeamSuccess(team)),
        catchError((error: HttpErrorResponse) => observableOf(new DeleteTeamFailure(error))))
    ));

  @Effect()
  deleteTeamSuccess$ = this.actions$.pipe(
    ofType(TeamActionTypes.DELETE_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Deleted team.'
    })));

  @Effect()
  deleteTeamFailure$ = this.actions$.pipe(
    ofType(TeamActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteTeamFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete team: ${msg || error}.`
      });
    }));

  @Effect()
  addTeamUsers$ = this.actions$.pipe(ofType<AddTeamUsers>(TeamActionTypes.ADD_USERS),
    mergeMap(({ payload }: AddTeamUsers) =>
      this.requests.addTeamUsers(payload).pipe(
        map((resp: UsersResponse) => new AddTeamUsersSuccess({...resp, id: payload.id})),
        catchError((error: HttpErrorResponse) => observableOf(new AddTeamUsersFailure(error))))
    ));

  @Effect()
  addTeamUsersSuccess$ = this.actions$.pipe(
    ofType(TeamActionTypes.ADD_USERS_SUCCESS),
    map(({ payload: { user_ids } }: AddTeamUsersSuccess ) => {
      const message = user_ids.length === 1 ? 'Added 1 user.' : `Added ${user_ids.length} users.`;
      return new CreateNotification({
        type: Type.info,
        message: message
      });
    }));

  @Effect()
  addTeamUsersFailure$ = this.actions$.pipe(
    ofType(TeamActionTypes.ADD_USERS_FAILURE),
    map(({ payload: { error } }: AddTeamUsersFailure) =>
      new CreateNotification({
        type: Type.error,
        message: `Could not add users to team: ${error.error || error}.`
      })));

  @Effect()
  removeTeamUsers$ = this.actions$.pipe(ofType<RemoveTeamUsers>(TeamActionTypes.REMOVE_USERS),
    mergeMap(({ payload }: RemoveTeamUsers) =>
      this.requests.removeTeamUsers(payload).pipe(
        map((resp: UsersResponse ) => new RemoveTeamUsersSuccess({...resp, id: payload.id})),
        catchError((error: HttpErrorResponse) =>
          observableOf(new RemoveTeamUsersFailure(error))))
    ));

  @Effect()
  removeTeamUsersSuccess$ = this.actions$.pipe(
    ofType(TeamActionTypes.REMOVE_USERS_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Removed user from team.'
    })));

  @Effect()
  removeTeamUsersFailure$ = this.actions$.pipe(
    ofType(TeamActionTypes.REMOVE_USERS_FAILURE),
    map(({ payload }: RemoveTeamUsersFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not remove user from team: ${msg || payload.error}.`
      });
    }));
}
