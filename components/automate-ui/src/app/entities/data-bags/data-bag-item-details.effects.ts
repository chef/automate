import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetDataBagItemDetails,
  GetDataBagItemDetailsSuccess,
  GetDataBagItemDetailsFailure,
  DataBagItemDetailsActionTypes
} from './data-bag-item-details.actions';

import { DataBagsRequests } from './data-bags.requests';

@Injectable()
export class DataBagItemDetailsEffects {
  constructor(
    private actions$: Actions,
    private requests: DataBagsRequests
  ) { }

  getDataBagItemDetails$ = createEffect(() =>
    this.actions$.pipe(
      ofType(DataBagItemDetailsActionTypes.GET),
      mergeMap(({ payload: { server_id, org_id, name, item_name } }: GetDataBagItemDetails) =>
        this.requests.getDataBagItemDetails(server_id, org_id, name, item_name).pipe(
          map((resp) => new GetDataBagItemDetailsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetDataBagItemDetailsFailure(error)))))));

    getDataBagItemDetailsFailure$ = createEffect(() =>
      this.actions$.pipe(
        ofType(DataBagItemDetailsActionTypes.GET_FAILURE),
        map(({ payload }: GetDataBagItemDetailsFailure) => {
          const msg = payload.error.error;
          return new CreateNotification({
            type: Type.error,
            message: `Could not get data bag item details: ${msg || payload.error}`
          });
      })));

}
