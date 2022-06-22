import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import { HttpStatus } from 'app/types/types';

import {
  GetDataBags,
  GetDataBagsSuccess,
  GetDataBagsFailure,
  DataBagsSuccessPayload,
  DataBagActionTypes,
  CreateDataBag,
  CreateDataBagSuccess,
  CreateDataBagFailure,
  DeleteDataBag,
  DeleteDataBagSuccess,
  DeleteDataBagFailure
} from './data-bags.actions';

import { DataBagsRequests } from './data-bags.requests';

@Injectable()
export class DataBagsEffects {
  constructor(
    private actions$: Actions,
    private requests: DataBagsRequests
  ) { }

  getDataBags$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.GET_ALL),
    mergeMap(({ payload: { server_id, org_id } }: GetDataBags) =>
      this.requests.getDataBags(server_id, org_id).pipe(
        map((resp: DataBagsSuccessPayload) => new GetDataBagsSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new GetDataBagsFailure(error)))))));

  getDataBagsFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetDataBagsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get data bags: ${msg || payload.error}`
      });
    })));

  createDataBag$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.CREATE),
    mergeMap(({ payload: { dataBag } }: CreateDataBag) =>
      this.requests.createDataBag(dataBag).pipe(
        map(() => new CreateDataBagSuccess({ databag: dataBag })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new CreateDataBagFailure(error)))))));

  createDataBagSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.CREATE_SUCCESS),
    map(({ payload: { databag: dataBag } }: CreateDataBagSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully created data bag - ${dataBag.name}.`
      });
    })));

  createDataBagFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateDataBagFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateDataBagFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could not create data bag: ${payload.error.error || payload}.`
      });
    })));

  deleteDataBag$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.DELETE),
    mergeMap(({ payload: { server_id, org_id, name } }: DeleteDataBag) =>
      this.requests.deleteDataBag(server_id, org_id, name).pipe(
        map(() => new DeleteDataBagSuccess({ name })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteDataBagFailure(error)))))));

  deleteDataBagSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }: DeleteDataBagSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully deleted data bag - ${name}.`
      });
    })));

  deleteDataBagFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteDataBagFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete data bag: ${msg || error}`
      });
    })));

}
