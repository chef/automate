import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
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
  DeleteEnvironmentFailure
} from './environment.action';

import { EnvironmentRequests } from './environment.requests';

@Injectable()
export class EnvironmentEffects {
  constructor(
    private actions$: Actions,
    private requests: EnvironmentRequests
  ) { }

  @Effect()
  getEnvironments$ = this.actions$.pipe(
    ofType(EnvironmentActionTypes.GET_ALL),
    mergeMap((action: GetEnvironments) =>
      this.requests.getEnvironments(action.payload).pipe(
        map((resp: GetEnvironmentsSuccessPayload) => new GetEnvironmentsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetEnvironmentsFailure(error))))));

  @Effect()
  getEnvironmentsFailure$ = this.actions$.pipe(
    ofType(EnvironmentActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetEnvironmentsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra Environment details: ${msg || payload.error}`
      });
    }));

  @Effect()
  getEnvironment$ = this.actions$.pipe(
    ofType(EnvironmentActionTypes.GET),
    mergeMap(({ payload: { server_id, org_id, name } }: GetEnvironment) =>
      this.requests.getEnvironment(server_id, org_id, name).pipe(
        map((resp) => new GetEnvironmentSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
        observableOf(new GetEnvironmentFailure(error))))));

  @Effect()
  getEnvironmentFailure$ = this.actions$.pipe(
    ofType(EnvironmentActionTypes.GET_FAILURE),
    map(({ payload }: GetEnvironmentFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get environment: ${msg || payload.error}`
      });
    }));

    @Effect()
    deleteEnvironment$ = this.actions$.pipe(
      ofType(EnvironmentActionTypes.DELETE),
      mergeMap(({ payload: { server_id, org_id, name } }: DeleteEnvironment) =>
        this.requests.deleteEnvironment(server_id, org_id, name).pipe(
          map(() => new DeleteEnvironmentSuccess({ name })),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteEnvironmentFailure(error))))));
  
    @Effect()
    deleteEnvironmentSuccess$ = this.actions$.pipe(
        ofType(EnvironmentActionTypes.DELETE_SUCCESS),
        map(({ payload: { name } }: DeleteEnvironmentSuccess) => {
          return new CreateNotification({
            type: Type.info,
            message: `Deleted environment ${name}.`
          });
        }));
  
    @Effect()
    deleteEnvironmentFailure$ = this.actions$.pipe(
      ofType(EnvironmentActionTypes.DELETE_FAILURE),
      map(({ payload: { error } }: DeleteEnvironmentFailure) => {
        const msg = error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete environment: ${msg || error}`
        });
      }));
}
