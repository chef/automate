import { combineLatest, of as observableOf } from 'rxjs';
import { Store } from '@ngrx/store';

import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { Router } from '@angular/router';
import { identity } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { HttpStatus } from 'app/types/types';
import { CreateNotification } from '../notifications/notification.actions';
import { Type } from '../notifications/notification.model';
import { iamMajorVersion } from 'app/entities/policies/policy.selectors';
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
import { TeamRequests, UsersResponse, TeamResponse, versionizeTeam } from './team.requests';
import { Team } from './team.model';

@Injectable()
export class TeamEffects {
  constructor(
    private actions$: Actions,
    private requests: TeamRequests,
    private router: Router,
    private store$: Store<NgrxStateAtom>
  ) { }

  @Effect()
  getTeams$ = combineLatest(
    this.actions$.pipe(ofType<GetTeams>(TeamActionTypes.GET_ALL)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([_action, version]) =>
        this.requests.getTeams(version).pipe(
          map((resp: GetTeamsSuccessPayload) => {
            const convertedTeams = resp.teams.map(team => versionizeTeam(team));
            return new GetTeamsSuccess({ teams: convertedTeams });
          }),
          catchError((error: HttpErrorResponse) => observableOf(new GetTeamsFailure(error))))));

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
  getTeam$ = combineLatest(
    this.actions$.pipe(ofType<GetTeam>(TeamActionTypes.GET)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload: { id } }, version]) =>
        this.requests.getTeam(id, version).pipe(
          map(({ team }: TeamResponse) => new GetTeamSuccess(versionizeTeam(team))),
          catchError((error: HttpErrorResponse) => observableOf(new GetTeamFailure(error))))));

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
  getTeamUsers$ = combineLatest(
    this.actions$.pipe(ofType<GetTeamUsers>(TeamActionTypes.GET_USERS)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload: { id } }, version]) =>
        this.requests.getTeamUsers(id, version).pipe(
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
  createTeam$ = combineLatest(
    this.actions$.pipe(ofType<CreateTeam>(TeamActionTypes.CREATE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload }, version]) =>
        this.requests.createTeam(payload, version).pipe(
          map(({ team }: TeamResponse) => new CreateTeamSuccess(versionizeTeam(team))),
          catchError((error) => observableOf(new CreateTeamFailure(error))))));

  @Effect()
  createTeamSuccess$ = combineLatest(
    this.actions$.pipe(ofType<CreateTeamSuccess>(TeamActionTypes.CREATE_SUCCESS)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      map(([{ payload }, version]) => {
        // Drop the user on the newly created team page.
        const id = version === 'v1' ? payload.guid : payload.id;
        this.router.navigate(['admin', 'teams', id]);
        return new CreateNotification({
          type: Type.info,
          message: 'Created a new team.'
        });
      }));

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
  updateTeam$ = combineLatest(
    this.actions$.pipe(ofType<UpdateTeam>(TeamActionTypes.UPDATE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload }, version]) =>
        this.requests.updateTeam(payload, version).pipe(
          map(({ team }: TeamResponse) => new UpdateTeamSuccess(versionizeTeam(team))),
          catchError((error) => observableOf(new UpdateTeamFailure(error))))));

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
  deleteTeam$ = combineLatest(
    this.actions$.pipe(ofType<DeleteTeam>(TeamActionTypes.DELETE)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload }, version ]) =>
        this.requests.deleteTeam(payload, version).pipe(
          map(({ team }: TeamResponse) => new DeleteTeamSuccess(versionizeTeam(team))),
          catchError((error: HttpErrorResponse) => observableOf(new DeleteTeamFailure(error))))));

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
  addTeamUsers$ = combineLatest(
    this.actions$.pipe(ofType<AddTeamUsers>(TeamActionTypes.ADD_USERS)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload }, version]) =>
        this.requests.addTeamUsers(payload, version).pipe(
          // TODO: v2 returns the team's users but v1 returns the team. Once v1 is removed,
          // update this to pass the response instead of the payload for more timely data.
          map((_resp: UsersResponse | Team) => new AddTeamUsersSuccess(payload)),
          catchError((error: HttpErrorResponse) => observableOf(new AddTeamUsersFailure(error))))));

  @Effect()
  addTeamUsersSuccess$ = this.actions$.pipe(
    ofType(TeamActionTypes.ADD_USERS_SUCCESS),
    map(() => new CreateNotification({
      type: Type.info,
      message: 'Added user(s) to team.'
    })));

  @Effect()
  addTeamUsersFailure$ = this.actions$.pipe(
    ofType(TeamActionTypes.ADD_USERS_FAILURE),
    map(({ payload }: AddTeamUsersFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not add user(s) to team: ${msg || payload.error}.`
      });
    }));

  @Effect()
  removeTeamUsers$ = combineLatest(
    this.actions$.pipe(ofType<RemoveTeamUsers>(TeamActionTypes.REMOVE_USERS)),
    this.store$.select(iamMajorVersion).pipe(filter(identity)))
    .pipe(
      mergeMap(([{ payload }, version ]) =>
        this.requests.removeTeamUsers(payload, version).pipe(
          // TODO: v2 returns the team's users but v1 returns the team. Once v1 is removed,
          // update this to pass the response instead of the payload for more timely data.
          map((_resp: UsersResponse | Team) => new RemoveTeamUsersSuccess(payload)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new RemoveTeamUsersFailure(error))))));

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
