import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetDataBagDetails,
  GetDataBagDetailsSuccess,
  GetDataBagDetailsFailure,
  DataBagDetailsActionTypes,
  DataBagSearchDetails,
  DataBagSearchSuccessPayload,
  DataBagSearchDetailsSuccess,
  DataBagSearchdDetailsFailure,
  DataBagItemListsSuccessPayload
} from './data-bag-details.actions';

import { DataBagsRequests } from './data-bags.requests';

@Injectable()
export class DataBagDetailsEffects {
  constructor(
    private actions$: Actions,
    private requests: DataBagsRequests
  ) { }

  @Effect()
  getDataBagDetails$ = this.actions$.pipe(
    ofType(DataBagDetailsActionTypes.GET_ALL),
    mergeMap(({ payload: { server_id, org_id, name } }: GetDataBagDetails) =>
      this.requests.getDataBagItemList(server_id, org_id, name).pipe(
        map((resp: DataBagItemListsSuccessPayload) => new GetDataBagDetailsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetDataBagDetailsFailure(error))))));

  @Effect()
  getDataBagDetailsFailure$ = this.actions$.pipe(
    ofType(DataBagDetailsActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetDataBagDetailsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra data bag details: ${msg || payload.error}`
      });
    }));


  @Effect()
  getDataBagSearchDetails$ = this.actions$.pipe(
    ofType(DataBagDetailsActionTypes.SEARCH),
    mergeMap((action: DataBagSearchDetails) =>
      this.requests.getDataBagSearchDetails(action.payload).pipe(
        map((resp: DataBagSearchSuccessPayload) => new DataBagSearchDetailsSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DataBagSearchdDetailsFailure(error))))));

  @Effect()
  getDataBagSearchDetailsFailure$ = this.actions$.pipe(
    ofType(DataBagDetailsActionTypes.SEARCH_FAILURE),
    map(({ payload }: DataBagSearchdDetailsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get infra data bag details: ${msg || payload.error}`
      });
    }));


}
