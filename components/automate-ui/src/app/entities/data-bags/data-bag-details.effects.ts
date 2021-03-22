import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';
import { DataBagsItemDetails } from './data-bags.model';
import { HttpStatus } from 'app/types/types';

import {
  GetDataBagItems,
  GetDataBagItemsSuccess,
  GetDataBagItemsFailure,
  DataBagItemsActionTypes,
  CreateDataBagItem,
  CreateDataBagItemSuccess,
  CreateDataBagItemSuccessPayload,
  CreateDataBagItemFailure,
  DeleteDataBagItem,
  DeleteDataBagItemSuccess,
  DeleteDataBagItemFailure,
  UpdateDataBagItem,
  UpdateDataBagItemFailure,
  UpdateDataBagItemSuccess,
  DataBagItemsSuccessPayload
} from './data-bag-details.actions';

import { DataBagsRequests } from './data-bags.requests';

@Injectable()
export class DataBagItemsEffects {
  constructor(
    private actions$: Actions,
    private requests: DataBagsRequests
  ) { }

  @Effect()
  getDataBagItems$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.GET_ALL),
    mergeMap(( action: GetDataBagItems) =>
      this.requests.getDataBagItems(action.payload).pipe(
        map((resp: DataBagItemsSuccessPayload) => new GetDataBagItemsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetDataBagItemsFailure(error))))));

  @Effect()
  getDataBagItemsFailure$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetDataBagItemsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra data bag items: ${msg || payload.error}`
      });
    }));

  @Effect()
  createDataBagItem$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.CREATE),
    mergeMap(({ payload: { dataBagItem } }: CreateDataBagItem) =>
      this.requests.createDataBagItem(dataBagItem).pipe(
        map((resp: CreateDataBagItemSuccessPayload) => new CreateDataBagItemSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new CreateDataBagItemFailure(error))))));

  @Effect()
  createDataBagItemSuccess$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.CREATE_SUCCESS),
    map(({ payload: { id: id } }: CreateDataBagItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully Created Data Bag Item ${id}.`
      });
    }));

  @Effect()
  createDataBagItemFailure$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateDataBagItemFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateDataBagItemFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could Not Create Data Bag Item: ${payload.error.error || payload}.`
      });
    }));

  @Effect()
  deleteDataBagItem$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.DELETE),
    mergeMap(({ payload: { server_id, org_id, databag_name, name } }: DeleteDataBagItem) =>
      this.requests.deleteDataBagItem(server_id, org_id, databag_name, name).pipe(
        map(() => new DeleteDataBagItemSuccess({ name })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteDataBagItemFailure(error))))));

  @Effect()
  deleteDataBagItemSuccess$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }: DeleteDataBagItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully Deleted Data Bag Item - ${name}.`
      });
    }));

  @Effect()
  deleteDataBagItemFailure$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteDataBagItemFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete data bag item: ${msg || error}`
      });
    }));

  @Effect()
  updateDataBagItem$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.UPDATE),
    mergeMap(({ payload: { dataBagItem } }: UpdateDataBagItem) =>
      this.requests.updateDataBagItem(dataBagItem).pipe(
        map((resp: DataBagsItemDetails) => new UpdateDataBagItemSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateDataBagItemFailure(error))))));

  @Effect()
  updateDataBagItemSuccess$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.UPDATE_SUCCESS),
    map(({ payload: dataBagItem }: UpdateDataBagItemSuccess) => new CreateNotification({
    type: Type.info,
    message: `Successfully Updated Data Bag Item - ${dataBagItem.item_id}.`
  })));

  @Effect()
  updateDataBagItemFailure$ = this.actions$.pipe(
    ofType(DataBagItemsActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateDataBagItemFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update data bag item: ${msg || payload.error}`
      });
    }));
}
