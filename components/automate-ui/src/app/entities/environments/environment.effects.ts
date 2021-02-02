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
  EnvironmentsSuccessPayload,
  GetEnvironmentsFailure,
  EnvironmentActionTypes,
  GetEnvironment,
  GetEnvironmentSuccess,
  GetEnvironmentFailure,
  EnvironmentSearch,
  EnvironmentSearchSuccess,
  EnvironmentSearchSuccessPayload,
  EnvironmentSearchFailure
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
    mergeMap(({ payload: { server_id, org_id } }: GetEnvironments) =>
      this.requests.getEnvironments(server_id, org_id).pipe(
        map((resp: EnvironmentsSuccessPayload) => new GetEnvironmentsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
        observableOf(new GetEnvironmentsFailure(error))))));

  @Effect()
  getEnvironmentsFailure$ = this.actions$.pipe(
    ofType(EnvironmentActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetEnvironmentsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get environments: ${msg || payload.error}`
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
    getEnvironmentSearch$ = this.actions$.pipe(
      ofType(EnvironmentActionTypes.SEARCH),
      mergeMap((action: EnvironmentSearch) =>
        this.requests.getEnvironmentSearch(action.payload).pipe(
          map((resp: EnvironmentSearchSuccessPayload) => new EnvironmentSearchSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new EnvironmentSearchFailure(error))))));

    @Effect()
    getEnvironmentSearchFailure$ = this.actions$.pipe(
      ofType(EnvironmentActionTypes.SEARCH_FAILURE),
      map(({ payload }: EnvironmentSearchFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get infra Environment details: ${msg || payload.error}`
        });
      }));
}
