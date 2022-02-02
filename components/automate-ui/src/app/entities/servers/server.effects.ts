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
  DeleteServerFailure,
  GetUsers,
  GetUsersSuccess,
  GetUsersFailure,
  UsersSuccessPayload,
  UpdateWebUIKey,
  UpdateWebUIKeySuccess,
  UpdateWebUIKeyFailure,
  ValidateWebUIKey,
  ValidateWebUIKeySuccess,
  ValidateWebUIKeyFailure,
  ValidateWebUIKeySuccessNot,
  GetMigrationStatus,
  GetMigrationStatusSuccess,
  GetMigrationStatusFailure
} from './server.actions';

import {
  ServerRequests,
  ServerResponse,
  ValidateWebUIKeyResponse
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

  getUsers$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServerActionTypes.GET_USERS),
    mergeMap((action: GetUsers) =>
      this.requests.getUser(action.payload).pipe(
        map((resp: UsersSuccessPayload) => new GetUsersSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new GetUsersFailure(error)))))));

  getUsersFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(ServerActionTypes.GET_USERS_FAILURE),
    map(({ payload: {error} }: GetUsersFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get users: ${msg || error}`
      });
    })));

  UpdateWebUIKey$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.UPDATE_WEB_UI_KEY),
    mergeMap(({ payload }: UpdateWebUIKey) =>
      this.requests.updateWebUIKey(payload).pipe(
        map((resp) => new UpdateWebUIKeySuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateWebUIKeyFailure(error)))))));

  UpdateWebUIKeySuccess$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.UPDATE_WEB_UI_KEY_SUCCESS),
    map(() => new CreateNotification({
    type: Type.info,
    message: 'Successfully updated Web UI Key.'
  }))));

  UpdateWebUIKeyFailure$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.UPDATE_WEB_UI_KEY_FAILURE),
    map(({ payload }: UpdateWebUIKeyFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update Web UI Key ${msg || payload.error}.`
      });
    })));

  ValidateWebUIKey$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.VALIDATE_WEB_UI_KEY),
    mergeMap(({ payload }: ValidateWebUIKey) =>
      this.requests.validateWebUIKey(payload).pipe(
        map((resp: ValidateWebUIKeyResponse) =>
        resp.valid ? new ValidateWebUIKeySuccess(resp) : new ValidateWebUIKeySuccessNot(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new ValidateWebUIKeyFailure(error))
      )))));

  ValidateWebUIKeySuccessNot$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.VALIDATE_WEB_UI_KEY_SUCCESS_NOT),
    map(({ payload }: ValidateWebUIKeySuccessNot) => {
      if (!payload.valid) {
        return new CreateNotification({
          type: Type.error,
          message: `Invalid webui key: ${payload.error}`
        });
      }
    })));

  ValidateWebUIKeyFailure$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.VALIDATE_WEB_UI_KEY_FAILURE),
    map(({ payload }: UpdateWebUIKeyFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not validated Web UI Key ${msg || payload.error}.`
      });
    })));

  GetMigrationStatus$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.GET_MIGRATION_STATUS),
    mergeMap(({ payload }: GetMigrationStatus) =>
      this.requests.getMigrationStatus(payload).pipe(
        map((resp) => new GetMigrationStatusSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetMigrationStatusFailure(error)))))));

  GetMigrationStatusSuccess$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.GET_MIGRATION_STATUS_SUCCESS),
    map(() => new CreateNotification({
    type: Type.info,
    message: 'Migration status updated.'
  }))));

  GetMigrationStatusFailure$ = createEffect(() => this.actions$.pipe(
    ofType(ServerActionTypes.GET_MIGRATION_STATUS_FAILURE),
    map(({ payload }: GetMigrationStatusFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update Migration status: ${msg || payload.error}.`
      });
    })));
}
