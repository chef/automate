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
  DataBagDetailsSuccessPayload,
  DataBagDetailsActionTypes
} from './data-bag-details.action';

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
        this.requests.getDataBagDetails(server_id, org_id, name).pipe(
          map((resp: DataBagDetailsSuccessPayload) => new GetDataBagDetailsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetDataBagDetailsFailure(error))))));

  @Effect()
  getDataBagsFailure$ = this.actions$.pipe(
      ofType(DataBagDetailsActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetDataBagDetailsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get infra data bag details: ${msg || payload.error}`
        });
      }));

}
