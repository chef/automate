import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetDataBagItems,
  GetDataBagItemsSuccess,
  GetDataBagItemsFailure,
  DataBagItemsActionTypes,
  DeleteDataBagItem,
  DeleteDataBagItemSuccess,
  DeleteDataBagItemFailure,
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

}
