import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, filter, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { HttpStatus } from 'app/types/types';
import { Type } from 'app/entities/notifications/notification.model';

import {
  CreateEnvironment,
  CreateEnvironmentSuccess,
  CreateEnvironmentFailure,
  GetEnvironments,
  GetEnvironmentsSuccess,
  GetEnvironmentsSuccessPayload,
  GetEnvironmentsFailure,
  EnvironmentActionTypes,
  GetEnvironment,
  GetEnvironmentSuccess,
  GetEnvironmentFailure,
  DeleteEnvironment,
  DeleteEnvironmentSuccess,
  DeleteEnvironmentFailure,
  UpdateEnvironment,
  UpdateEnvironmentSuccess,
  UpdateEnvironmentFailure
} from './environment.action';

import { EnvironmentRequests, EnvironmentResponse } from './environment.requests';
import { Environment } from './environment.model';

@Injectable()
export class EnvironmentEffects {
  constructor(
    private actions$: Actions,
    private requests: EnvironmentRequests
  ) { }

  getEnvironments$ = createEffect(() =>
    this.actions$.pipe(
    ofType(EnvironmentActionTypes.GET_ALL),
    mergeMap((action: GetEnvironments) =>
      this.requests.getEnvironments(action.payload).pipe(
        map((resp: GetEnvironmentsSuccessPayload) => new GetEnvironmentsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetEnvironmentsFailure(error)))))));

  getEnvironmentsFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(EnvironmentActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetEnvironmentsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get environment details: ${msg || payload.error}`
      });
    })));

  getEnvironment$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.GET),
      mergeMap(({ payload: { server_id, org_id, name } }: GetEnvironment) =>
        this.requests.getEnvironment(server_id, org_id, name).pipe(
          map((resp) => new GetEnvironmentSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetEnvironmentFailure(error)))))));

  getEnvironmentFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.GET_FAILURE),
      map(({ payload }: GetEnvironmentFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get environment: ${msg || payload.error}`
        });
    })));

  deleteEnvironment$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.DELETE),
      mergeMap(({ payload: { server_id, org_id, name } }: DeleteEnvironment) =>
        this.requests.deleteEnvironment(server_id, org_id, name).pipe(
          map(() => new DeleteEnvironmentSuccess({ name })),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteEnvironmentFailure(error)))))));

  deleteEnvironmentSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.DELETE_SUCCESS),
      map(({ payload: { name } }: DeleteEnvironmentSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Successfully Deleted Environment - ${name}.`
        });
    })));

  deleteEnvironmentFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.DELETE_FAILURE),
      map(({ payload: { error } }: DeleteEnvironmentFailure) => {
        const msg = error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete environment: ${msg || error}`
        });
      })));

  createEnvironment$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.CREATE),
      mergeMap(({ payload: { environment } }: CreateEnvironment) =>
        this.requests.createEnvironment(environment).pipe(
          map((resp: EnvironmentResponse) => new CreateEnvironmentSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new CreateEnvironmentFailure(error)))))));

  createEnvironmentSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.CREATE_SUCCESS),
      map(({ payload: { name } }: CreateEnvironmentSuccess) => new CreateNotification({
        type: Type.info,
        message: `Created environment ${name}.`
      }))));

  createEnvironmentFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.CREATE_FAILURE),
      filter(({ payload }: CreateEnvironmentFailure) => payload.status !== HttpStatus.CONFLICT),
      map(({ payload }: CreateEnvironmentFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create environment: ${payload.error.error || payload}.`
      }))));

  updateEnvironment$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.UPDATE),
      mergeMap(({ payload }: UpdateEnvironment) =>
        this.requests.updateEnvironment(payload).pipe(
          map((resp: Environment) => new UpdateEnvironmentSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new UpdateEnvironmentFailure(error)))))));

  updateEnvironmentSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.UPDATE_SUCCESS),
      map(({ payload: environment }: UpdateEnvironmentSuccess) => new CreateNotification({
        type: Type.info,
        message: `Updated environment ${environment.name}.`
      }))));

  updateEnvironmentFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(EnvironmentActionTypes.UPDATE_FAILURE),
      filter(({ payload }: UpdateEnvironmentFailure) => payload.status !== HttpStatus.CONFLICT),
      map(({ payload }: UpdateEnvironmentFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not update environment: ${payload.error.error || payload}.`
      }))));

}
