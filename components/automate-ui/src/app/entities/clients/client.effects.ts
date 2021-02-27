import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetClients,
  GetClientsSuccess,
  ClientsSuccessPayload,
  GetClientsFailure,
  ClientActionTypes,
  GetClient,
  GetClientSuccess,
  GetClientFailure,
  DeleteClient,
  DeleteClientSuccess,
  DeleteClientFailure
} from './client.action';

import { ClientRequests } from './client.requests';

@Injectable()
export class ClientEffects {
  constructor(
    private actions$: Actions,
    private requests: ClientRequests
  ) { }

  @Effect()
  getClients$ = this.actions$.pipe(
    ofType(ClientActionTypes.GET_ALL),
    mergeMap((action: GetClients) =>
    this.requests.getClients(action.payload).pipe(
        map((resp: ClientsSuccessPayload) => new GetClientsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
        observableOf(new GetClientsFailure(error))))));

  @Effect()
  getClientsFailure$ = this.actions$.pipe(
    ofType(ClientActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetClientsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get clients: ${msg || payload.error}`
      });
    }));

  @Effect()
  getClient$ = this.actions$.pipe(
    ofType(ClientActionTypes.GET),
    mergeMap(({ payload: { server_id, org_id, name } }: GetClient) =>
      this.requests.getClient(server_id, org_id, name).pipe(
        map((resp) => new GetClientSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
        observableOf(new GetClientFailure(error))))));

  @Effect()
  getClientFailure$ = this.actions$.pipe(
    ofType(ClientActionTypes.GET_FAILURE),
    map(({ payload }: GetClientFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get client: ${msg || payload.error}`
      });
    }));
  
  @Effect()
  deleteClient$ = this.actions$.pipe(
    ofType(ClientActionTypes.DELETE),
    mergeMap(({ payload: { server_id, org_id, name } }: DeleteClient) =>
      this.requests.deleteClient(server_id, org_id, name).pipe(
        map(() => new DeleteClientSuccess({ name })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteClientFailure(error))))));

  @Effect()
  deleteClientSuccess$ = this.actions$.pipe(
    ofType(ClientActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }: DeleteClientSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Deleted client ${name}.`
      });
    }));

  @Effect()
  deleteClientFailure$ = this.actions$.pipe(
    ofType(ClientActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteClientFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete client: ${msg || error}`
      });
    }));
}
