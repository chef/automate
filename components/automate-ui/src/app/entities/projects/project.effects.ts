import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Store } from '@ngrx/store';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { interval as observableInterval, of as observableOf, Observable } from 'rxjs';
import { catchError, mergeMap, map, filter, switchMap, withLatestFrom } from 'rxjs/operators';
import { get } from 'lodash/fp';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import { allPerms } from 'app/entities/userperms/userperms.selectors';
import { ProjectRequests } from './project.requests';

import {
  GetProjects,
  GetProjectsSuccess,
  GetProjectsSuccessPayload,
  GetProjectsFailure,
  GetProject,
  GetProjectSuccess,
  GetProjectFailure,
  CreateProject,
  CreateProjectSuccess,
  CreateProjectFailure,
  DeleteProject,
  DeleteProjectSuccess,
  DeleteProjectFailure,
  UpdateProject,
  UpdateProjectFailure,
  UpdateProjectSuccess,
  ApplyRulesStart,
  ApplyRulesStartSuccess,
  ApplyRulesStartFailure,
  ApplyRulesStop,
  ApplyRulesStopSuccess,
  ApplyRulesStopFailure,
  GetApplyRulesStatus,
  GetApplyRulesStatusSuccess,
  GetApplyRulesStatusFailure,
  ProjectSuccessPayload,
  ProjectActionTypes,
  ProjectActions
} from './project.actions';
import {
  LoadOptions
} from 'app/services/projects-filter/projects-filter.actions';
import { applyRulesStatus } from './project.selectors';
import { ApplyRulesStatusState } from './project.reducer';

const ACTIVE_RULE_STATUS_INTERVAL = 5; // seconds between checks while update is in progress
const DORMANT_RULE_STATUS_INTERVAL = 120; // seconds between checks while update is dormant

@Injectable()
export class ProjectEffects {
  constructor(
    private actions$: Actions,
    private requests: ProjectRequests,
    private store: Store<NgrxStateAtom>
  ) { }

  getProjects$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.GET_ALL),
      mergeMap(() =>
        this.requests.getProjects().pipe(
          map((resp: GetProjectsSuccessPayload) => new GetProjectsSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetProjectsFailure(error)))))));

  getProjectsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetProjectsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get projects: ${msg || payload.error}`
        });
      })));

  getProject$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.GET),
      mergeMap(({ payload: { id } }: GetProject) =>
        this.requests.getProject(id).pipe(
          map((resp: ProjectSuccessPayload) => new GetProjectSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetProjectFailure(error)))))));

  getProjectFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.GET_FAILURE),
      map(({ payload }: GetProjectFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get project: ${msg || payload.error}`
        });
      })));

  createProject$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.CREATE),
      mergeMap(({ payload }: CreateProject) =>
        this.requests.createProject(payload.id, payload.name, payload.skip_policies).pipe(
          map((resp: ProjectSuccessPayload) => {
            const skip_policies = payload.skip_policies;
            return new CreateProjectSuccess({resp, skip_policies});
          }),
          catchError((error: HttpErrorResponse) =>
            observableOf(new CreateProjectFailure(error)))))));

  createProjectSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.CREATE_SUCCESS),
      map(({ payload: { resp, skip_policies } }: CreateProjectSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: skip_policies
            ? `Created project ${resp.project.id}.`
            : `Created project ${resp.project.id} and associated policies.`
        });
    })));

  createProjectFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ProjectActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateProjectFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateProjectFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create project: ${payload.error.error || payload}`
      }))));

  deleteProject$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.DELETE),
      mergeMap(({ payload: { id } }: DeleteProject) =>
        this.requests.deleteProject(id).pipe(
          map(() => new DeleteProjectSuccess({id})),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteProjectFailure(error)))))));

  deleteProjectSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.DELETE_SUCCESS),
      switchMap(({ payload: { id } }: DeleteProjectSuccess) => [
        new LoadOptions(),
        new CreateNotification({
          type: Type.info,
          message: `Deleted project ${id}.`
        })
      ])));

  deleteProjectFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.DELETE_FAILURE),
      map(({ payload }: DeleteProjectFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete project: ${msg || payload.error}`
        });
      })));

  updateProject$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.UPDATE),
      mergeMap(({ payload: { id, name } }: UpdateProject) =>
        this.requests.updateProject(id, name).pipe(
          map((resp: ProjectSuccessPayload) => new UpdateProjectSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateProjectFailure(error)))))));

  updateProjectFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ProjectActionTypes.UPDATE_FAILURE),
      map(({ payload }: UpdateProjectFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not update project: ${msg || payload.error}`
        });
      })));

  applyRulesStart$ = createEffect(() =>
    this.actions$.pipe(
    ofType<ApplyRulesStart>(ProjectActionTypes.APPLY_RULES_START),
    mergeMap(() =>
      this.requests.applyRulesStart().pipe(
        switchMap(() => [
          new ApplyRulesStartSuccess(),
          new GetProjects(),
          new GetApplyRulesStatus()
        ]),
        catchError((error: HttpErrorResponse) =>
          observableOf(new ApplyRulesStartFailure(error)))))));

  applyRulesStop$ = createEffect(() =>
    this.actions$.pipe(
    ofType<ApplyRulesStop>(ProjectActionTypes.APPLY_RULES_STOP),
    mergeMap(() =>
      this.requests.applyRulesStop().pipe(
        switchMap(() => [
          new ApplyRulesStopSuccess(),
          new GetProjects(),
          new GetApplyRulesStatus()
        ]),
        catchError(
          (error: HttpErrorResponse) => observableOf(new ApplyRulesStopFailure(error)))))));

  getApplyRulesStatus$ = createEffect(() =>
    this.actions$.pipe(
    ofType<GetApplyRulesStatus>(ProjectActionTypes.GET_APPLY_RULES_STATUS),
    switchMap(this.getRulesStatus$())));

  getActiveApplyRulesStatus$ = createEffect(() =>
    observableInterval(1000 * ACTIVE_RULE_STATUS_INTERVAL).pipe(
      withLatestFrom(this.store.select(applyRulesStatus)),
      filter(([_, { state }]) =>
        state === ApplyRulesStatusState.Running
      ),
      switchMap(this.getRulesStatus$())));

  getDormantApplyRulesStatus$ = createEffect(() =>
    observableInterval(1000 * DORMANT_RULE_STATUS_INTERVAL).pipe(
      withLatestFrom(this.store.select(allPerms)),
      withLatestFrom(this.store.select(applyRulesStatus)),
      filter(([[_interval, _allPerms], { state }]) =>
        state === ApplyRulesStatusState.NotRunning
      ),
      switchMap(([[_interval, list], _status]) => {
        return get(['/apis/iam/v2/projects', 'get'], list) ?
          [new GetProjects(), new GetApplyRulesStatus()] :
          [new GetApplyRulesStatus()];
      }),
      catchError((error: HttpErrorResponse) =>
        observableOf(new GetApplyRulesStatusFailure(error)))));

  private getRulesStatus$(): () => Observable<ProjectActions> {
    return () => this.requests.getApplyRulesStatus().pipe(
      map((resp) => new GetApplyRulesStatusSuccess(resp)),
      catchError((error: HttpErrorResponse) =>
        observableOf(new GetApplyRulesStatusFailure(error))));
  }

}
