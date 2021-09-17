import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
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
  CreateServerFailure,
  UpdateServer,
  UpdateServerSuccess,
  UpdateServerFailure,
  DeleteServer,
  DeleteServerSuccess,
  DeleteServerFailure
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

  getServers$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.GET_ALL),
      mergeMap(() =>
        this.requests.getServers().pipe(
          map((resp: GetServersSuccessPayload) => new GetServersSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetServersFailure(error)))))));

  getServersFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetServersFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get servers: ${msg || payload.error}`
        });
      })));

  getServer$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.GET),
      mergeMap(({ payload: { id }}: GetServer) =>
        this.requests.getServer(id).pipe(
          map((resp: ServerResponse) => new GetServerSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetServerFailure(error, id)))))));

  getServerFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.GET_FAILURE),
      map(({ payload, id }: GetServerFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get server ${id}: ${msg || payload.error}`
        });
      })));

  createServer$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.CREATE),
      mergeMap(({ payload }: CreateServer) =>
      this.requests.createServer(payload).pipe(
        map((resp: ServerSuccessPayload) => new CreateServerSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new CreateServerFailure(error)))))));

  createServerSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.CREATE_SUCCESS),
      map(({ payload: { server } }: CreateServerSuccess) => new CreateNotification({
      type: Type.info,
      message: `Successfully created server - ${server.name}`
    }))));

  createServerFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServerActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateServerFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateServerFailure) => {
      const msg = payload.status ===
        HttpStatus.TIME_OUT_ERROR ? 'Gateway Time out ' : payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not create server: ${msg || payload}`
      });
    })));

  updateServer$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServerActionTypes.UPDATE),
    mergeMap(({ payload: { server } }: UpdateServer) =>
      this.requests.updateServer(server).pipe(
        map((resp: ServerSuccessPayload) => new UpdateServerSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateServerFailure(error)))))));

  updateOrgSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.UPDATE_SUCCESS),
      map(({ payload: { server } }: UpdateServerSuccess) => new CreateNotification({
      type: Type.info,
      message: `Successfully updated server - ${server.name}.`
    }))));

  updateServerFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServerActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateServerFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update server: ${msg || payload.error}`
      });
    })));

  deleteServer$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServerActionTypes.DELETE),
    mergeMap(({ payload: { id, name } }: DeleteServer) =>
      this.requests.deleteServer(id).pipe(
        map(() => new DeleteServerSuccess({id, name})),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteServerFailure(error)))))));

  deleteServerSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ServerActionTypes.DELETE_SUCCESS),
      map(({ payload: { name } }: DeleteServerSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Successfully deleted server - ${name}.`
        });
      })));

  deleteServerFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServerActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteServerFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete server: ${msg || error}`
      });
    })));

}
