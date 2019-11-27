import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  ServerSuccessPayload,
  GetServersSuccess,
  GetServersSuccessPayload,
  GetServersFailure,
  ServerActionTypes,
  GetServer,
  GetServerSuccess,
  GetServerFailure,
  CreateServer,
  CreateServerSuccess,
  CreateServerFailure
} from './server.actions';

import {
  ServerRequests,
  ServerResponse
} from './server.requests';

@Injectable()
export class ServerEffects {
  constructor(
    private actions$: Actions,
    private requests: ServerRequests
  ) { }

  @Effect()
  getServers$ = this.actions$.pipe(
      ofType(ServerActionTypes.GET_ALL),
      mergeMap(() =>
        this.requests.getServers().pipe(
          map((resp: GetServersSuccessPayload) => new GetServersSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetServersFailure(error))))));

  @Effect()
  getServersFailure$ = this.actions$.pipe(
      ofType(ServerActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetServersFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get servers: ${msg || payload.error}`
        });
      }));

  @Effect()
  getServer$ = this.actions$.pipe(
      ofType(ServerActionTypes.GET),
      mergeMap(({ payload: { id }}: GetServer) =>
        this.requests.getServer(id).pipe(
          map((resp: ServerResponse) => new GetServerSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetServerFailure(error, id))))));

  @Effect()
  getServerFailure$ = this.actions$.pipe(
      ofType(ServerActionTypes.GET_FAILURE),
      map(({ payload, id }: GetServerFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get server ${id}: ${msg || payload.error}`
        });
      }));

  @Effect()
  createServer$ = this.actions$.pipe(
      ofType(ServerActionTypes.CREATE),
      mergeMap(({ payload }: CreateServer) =>
      this.requests.createServer(payload).pipe(
        map((resp: ServerSuccessPayload) => new CreateServerSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new CreateServerFailure(error))))));

  @Effect()
  createServerSuccess$ = this.actions$.pipe(
      ofType(ServerActionTypes.CREATE_SUCCESS),
      map(({ payload: { server } }: CreateServerSuccess) => new CreateNotification({
      type: Type.info,
      message: `Created server ${server.name}`
    })));

  @Effect()
  createServerFailure$ = this.actions$.pipe(
    ofType(ServerActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateServerFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateServerFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create server: ${payload.error.error || payload}`
      })));
}
