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
  ClientSearch,
  ClientSearchSuccess,
  ClientSearchSuccessPayload,
  ClientSearchFailure
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
    mergeMap(({ payload: { server_id, org_id } }: GetClients) =>
      this.requests.getClients(server_id, org_id).pipe(
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
    getClientSearch$ = this.actions$.pipe(
      ofType(ClientActionTypes.SEARCH),
      mergeMap((action: ClientSearch) =>
        this.requests.getClientSearch(action.payload).pipe(
          map((resp: ClientSearchSuccessPayload) => new ClientSearchSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new ClientSearchFailure(error))))));

    @Effect()
    getClientSearchFailure$ = this.actions$.pipe(
      ofType(ClientActionTypes.SEARCH_FAILURE),
      map(({ payload }: ClientSearchFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get infra clients details: ${msg || payload.error}`
        });
      }));
}
