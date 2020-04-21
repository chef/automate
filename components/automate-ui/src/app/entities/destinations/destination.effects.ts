import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  DestinationSuccessPayload,
  GetDestinationsSuccess,
  GetDestinationsSuccessPayload,
  GetDestinationsFailure,
  DestinationActionTypes,
  GetDestination,
  GetDestinationSuccess,
  GetDestinationFailure,
  CreateDestination,
  CreateDestinationSuccess,
  CreateDestinationFailure,
  UpdateDestination,
  UpdateDestinationSuccess,
  UpdateDestinationFailure,
  DeleteDestination,
  DeleteDestinationSuccess,
  DeleteDestinationFailure
} from './destination.actions';

import {
  DestinationRequests
} from './destination.requests';
import { Destination } from './destination.model';

@Injectable()
export class DestinationEffects {
  constructor(
    private actions$: Actions,
    private requests: DestinationRequests
  ) { }

  @Effect()
  getDestinations$ = this.actions$.pipe(
      ofType(DestinationActionTypes.GET_ALL),
      mergeMap(() =>
        this.requests.getDestinations().pipe(
          map((resp: GetDestinationsSuccessPayload) => new GetDestinationsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new GetDestinationsFailure(error))))));

  @Effect()
  getDestinationsFailure$ = this.actions$.pipe(
      ofType(DestinationActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetDestinationsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get destinations: ${msg || payload.error}`
        });
      }));

  @Effect()
  getDestination$ = this.actions$.pipe(
      ofType(DestinationActionTypes.GET),
      mergeMap(({ payload: { id }}: GetDestination) =>
        this.requests.getDestination(id).pipe(
          map((resp: Destination) => new GetDestinationSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetDestinationFailure(error, id))))));

  @Effect()
  getDestinationFailure$ = this.actions$.pipe(
      ofType(DestinationActionTypes.GET_FAILURE),
      map(({ payload, id }: GetDestinationFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get data feed ${id}: ${msg || payload.error}`
        });
      }));

  @Effect()
  createDestination$ = this.actions$.pipe(
      ofType(DestinationActionTypes.CREATE),
      mergeMap(({ payload, username, password }: CreateDestination) =>
      this.requests.createDestination( payload, username, password ).pipe(
        map((resp: DestinationSuccessPayload) => new CreateDestinationSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new CreateDestinationFailure(error))))));

  @Effect()
  createDestinationSuccess$ = this.actions$.pipe(
      ofType(DestinationActionTypes.CREATE_SUCCESS),
      map(({ payload  }: CreateDestinationSuccess) => new CreateNotification({
      type: Type.info,
      message: `Created data feed ${payload.name}`
    })));

  @Effect()
  createDestinationFailure$ = this.actions$.pipe(
    ofType(DestinationActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateDestinationFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateDestinationFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create data feed: ${payload.error.error || payload}`
      })));

  @Effect()
  updateDestination$ = this.actions$.pipe(
    ofType(DestinationActionTypes.UPDATE),
    mergeMap(({ payload: { destination } }: UpdateDestination) =>
      this.requests.updateDestination(destination).pipe(
        map((resp) => new UpdateDestinationSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateDestinationFailure(error))))));

  @Effect()
  updateDestinationSuccess$ = this.actions$.pipe(
      ofType(DestinationActionTypes.UPDATE_SUCCESS),
      map(({ payload  }: UpdateDestinationSuccess) => new CreateNotification({
      type: Type.info,
      message: `Updated data feed ${payload.name}.`
    })));

  @Effect()
  updateDestinationFailure$ = this.actions$.pipe(
    ofType(DestinationActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateDestinationFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update data feed: ${msg || payload.error}`
      });
    }));

  @Effect()
  deleteDestination$ = this.actions$.pipe(
    ofType(DestinationActionTypes.DELETE),
    mergeMap(({ payload: {id, name} }: DeleteDestination) =>
      this.requests.deleteDestination(id).pipe(
        map(() => new DeleteDestinationSuccess({ id, name })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteDestinationFailure(error))))));

  @Effect()
  deleteDestinationSuccess$ = this.actions$.pipe(
      ofType(DestinationActionTypes.DELETE_SUCCESS),
      map(({ payload: { name } }: DeleteDestinationSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Deleted data feed ${name}.`
        });
      }));

  @Effect()
  deleteDestinationFailure$ = this.actions$.pipe(
    ofType(DestinationActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteDestinationFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete data feed: ${msg || error}`
      });
    }));
}
