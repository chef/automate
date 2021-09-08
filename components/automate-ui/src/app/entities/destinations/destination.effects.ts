import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
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
  DeleteDestinationFailure,
  TestDestination,
  TestDestinationSuccess,
  TestDestinationFailure,
  EnableDisableDestination,
  EnableDisableDestinationSuccess,
  EnableDisableDestinationFailure
} from './destination.actions';

import {
  DestinationRequests
} from './destination.requests';
import { Destination } from './destination.model';
import { Router } from '@angular/router';

@Injectable()
export class DestinationEffects {
  constructor(
    private actions$: Actions,
    private router: Router,
    private requests: DestinationRequests
  ) { }

  getDestinations$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.GET_ALL),
      mergeMap(() =>
        this.requests.getDestinations().pipe(
          map((resp: GetDestinationsSuccessPayload) => new GetDestinationsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new GetDestinationsFailure(error)))))));

  getDestinationsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetDestinationsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get destinations: ${msg || payload.error}`
        });
      })));

  getDestination$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.GET),
      mergeMap(({ payload: { id }}: GetDestination) =>
        this.requests.getDestination(id).pipe(
          map((resp: Destination) => new GetDestinationSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetDestinationFailure(error, id)))))));

  getDestinationFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.GET_FAILURE),
      map(({ payload, id }: GetDestinationFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get data feed ${id}: ${msg || payload.error}`
        });
      })));

  createDestination$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.CREATE),
      mergeMap(({ payload, headers, storage }: CreateDestination) =>
      this.requests.createDestination( payload, headers, storage ).pipe(
        map((resp: DestinationSuccessPayload) => new CreateDestinationSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new CreateDestinationFailure(error)))))));

  createDestinationSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.CREATE_SUCCESS),
      map(({ payload }: CreateDestinationSuccess) => {
        this.router.navigate(['/settings/data-feeds/', payload.id]);
        return new CreateNotification({
          type: Type.info,
          message: `Created data feed ${payload.name}.`
        });
      })
    ));

  createDestinationFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateDestinationFailure) => payload.status !== HttpStatus.CONFLICT)),
    { dispatch: false });

  updateDestination$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.UPDATE),
    mergeMap(({ payload: { destination } }: UpdateDestination) =>
      this.requests.updateDestination(destination).pipe(
        map((resp) => new UpdateDestinationSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateDestinationFailure(error)))))));

  updateDestinationSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.UPDATE_SUCCESS),
      map(({ payload  }: UpdateDestinationSuccess) => new CreateNotification({
      type: Type.info,
      message: `Updated data feed ${payload.name}.`
    }))));

  updateDestinationFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateDestinationFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update data feed: ${msg || payload.error}.`
      });
    })));

  deleteDestination$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.DELETE),
    mergeMap(({ payload: {id, name} }: DeleteDestination) =>
      this.requests.deleteDestination(id).pipe(
        map(() => new DeleteDestinationSuccess({ id, name })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteDestinationFailure(error)))))));

  deleteDestinationSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.DELETE_SUCCESS),
      map(({ payload: { name } }: DeleteDestinationSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Deleted data feed ${name}.`
        });
      })));

  deleteDestinationFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteDestinationFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete data feed: ${msg || error}.`
      });
    })));

  testDestination$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.SEND_TEST),
    mergeMap(({ payload: { destination } }: TestDestination) =>
      this.requests.testDestination(destination).pipe(
        map(() => new TestDestinationSuccess(destination)),
        catchError(() =>
          observableOf(new TestDestinationFailure(destination)))))));

  testDestinationSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.SEND_TEST_SUCCESS),
      map(({ payload  }: TestDestinationSuccess) => new CreateNotification({
      type: Type.info,
      message: `Data feed test connected successfully for ${payload.name}.`
    }))));

  testDestinationFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DestinationActionTypes.SEND_TEST_FAILURE),
      map(({ payload }: TestDestinationFailure) => {
        return new CreateNotification({
          type: Type.error,
          message: `Unable to connect to data feed ${payload.name}.`
        });
    })));

  enableDisableDestination$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.ENABLE_DISABLE),
    mergeMap(( {payload:  {enableDisable} }: EnableDisableDestination) =>
    this.requests.enableDisableDestinations(enableDisable).pipe(
      map((resp: DestinationSuccessPayload) => new EnableDisableDestinationSuccess(resp)),
      catchError((error: HttpErrorResponse) =>
        observableOf(new GetDestinationsFailure(error)))))));

  enableDisableDestinationFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.ENABLE_DISABLE_FAILURE),
    map(({ payload  }: EnableDisableDestinationFailure) => new CreateNotification({
      type: Type.info,
      message: `Could not enable or Disable: error ${payload.error}.`
  }))));

  enableDisableDestinationSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DestinationActionTypes.ENABLE_DISABLE_SUCCESS),
    map(({ payload  }: EnableDisableDestinationSuccess) => new CreateNotification({
      type: Type.info,
      message: `Destination is ${ payload.enable ? 'Enabled' : 'Disabled' }.`
  }))));

}
