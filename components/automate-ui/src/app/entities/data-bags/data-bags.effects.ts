import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
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

  @Effect()
  getDataBags$ = this.actions$.pipe(
    ofType(DataBagActionTypes.GET_ALL),
    mergeMap(({ payload: { server_id, org_id } }: GetDataBags) =>
      this.requests.getDataBags(server_id, org_id).pipe(
        map((resp: DataBagsSuccessPayload) => new GetDataBagsSuccess(resp)),
        catchError((error: HttpErrorResponse) => observableOf(new GetDataBagsFailure(error))))));

  @Effect()
  getDataBagsFailure$ = this.actions$.pipe(
    ofType(DataBagActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetDataBagsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra data bags: ${msg || payload.error}`
      });
    }));

  @Effect()
  createDataBag$ = this.actions$.pipe(
    ofType(DataBagActionTypes.CREATE),
    mergeMap(({ payload: { dataBag } }: CreateDataBag) =>
      this.requests.createDataBag(dataBag).pipe(
        map(() => new CreateDataBagSuccess({ databag: dataBag })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new CreateDataBagFailure(error))))));

  @Effect()
  createDataBagSuccess$ = this.actions$.pipe(
    ofType(DataBagActionTypes.CREATE_SUCCESS),
    map(({ payload: { databag: dataBag } }: CreateDataBagSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully Created Data Bag ${dataBag.name}.`
      });
    }));

  @Effect()
  createDataBagFailure$ = this.actions$.pipe(
    ofType(DataBagActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateDataBagFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateDataBagFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could Not Create Data Bag: ${payload.error.error || payload}.`
      });
    }));

  @Effect()
  deleteDataBag$ = this.actions$.pipe(
    ofType(DataBagActionTypes.DELETE),
    mergeMap(({ payload: { server_id, org_id, name } }: DeleteDataBag) =>
      this.requests.deleteDataBag(server_id, org_id, name).pipe(
        map(() => new DeleteDataBagSuccess({ name })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteDataBagFailure(error))))));

  @Effect()
  deleteDataBagSuccess$ = this.actions$.pipe(
    ofType(DataBagActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }: DeleteDataBagSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully Deleted Data Bag - ${name}.`
      });
    }));

  @Effect()
  deleteDataBagFailure$ = this.actions$.pipe(
    ofType(DataBagActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteDataBagFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete data bag: ${msg || error}`
      });
    }));
}
