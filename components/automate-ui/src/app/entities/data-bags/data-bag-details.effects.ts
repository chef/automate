import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
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

  getDataBagItems$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagItemsActionTypes.GET_ALL),
    mergeMap(( action: GetDataBagItems) =>
      this.requests.getDataBagItems(action.payload).pipe(
        map((resp: DataBagItemsSuccessPayload) => new GetDataBagItemsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetDataBagItemsFailure(error)))))));

  getDataBagItemsFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(DataBagItemsActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetDataBagItemsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get data bag items: ${msg || payload.error}`
      });
    })));

  createDataBagItem$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.CREATE),
    mergeMap(({ payload: { dataBagItem } }: CreateDataBagItem) =>
      this.requests.createDataBagItem(dataBagItem).pipe(
        map((resp: CreateDataBagItemSuccessPayload) => new CreateDataBagItemSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new CreateDataBagItemFailure(error)))))));

  createDataBagItemSuccess$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.CREATE_SUCCESS),
    map(({ payload: { id: id } }: CreateDataBagItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully created data bag item ${id}.`
      });
    })));

  createDataBagItemFailure$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateDataBagItemFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateDataBagItemFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could not create data bag item: ${payload.error.error || payload}.`
      });
    })));

  deleteDataBagItem$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.DELETE),
    mergeMap(({ payload: { server_id, org_id, databag_name, name } }: DeleteDataBagItem) =>
      this.requests.deleteDataBagItem(server_id, org_id, databag_name, name).pipe(
        map(() => new DeleteDataBagItemSuccess({ name })),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteDataBagItemFailure(error)))))));

  deleteDataBagItemSuccess$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }: DeleteDataBagItemSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Successfully deleted data bag item - ${name}.`
      });
    })));

  deleteDataBagItemFailure$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.DELETE_FAILURE),
    map(({ payload: { error } }: DeleteDataBagItemFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete data bag item: ${msg || error}`
      });
    })));

  updateDataBagItem$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.UPDATE),
    mergeMap(({ payload: { dataBagItem } }: UpdateDataBagItem) =>
      this.requests.updateDataBagItem(dataBagItem).pipe(
        map((resp: DataBagsItemDetails) => new UpdateDataBagItemSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateDataBagItemFailure(error)))))));

  updateDataBagItemSuccess$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.UPDATE_SUCCESS),
    map(({ payload: dataBagItem }: UpdateDataBagItemSuccess) => new CreateNotification({
    type: Type.info,
    message: `Successfully updated data bag item - ${dataBagItem.item_id}.`
  }))));

  updateDataBagItemFailure$ = createEffect(() => this.actions$.pipe(
    ofType(DataBagItemsActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateDataBagItemFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update data bag item: ${msg || payload.error}`
      });
    })));
}
