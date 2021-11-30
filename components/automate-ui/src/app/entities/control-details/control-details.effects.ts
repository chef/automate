import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  ControlDetailActionTypes,
  GetControlDetail,
  GetControlDetailSuccess,
  GetControlDetailFailure
  // ,
  // ControlDetailSuccessPayload
} from './control-details.action';

import { ControlDetailRequests } from './control-details.requests';

@Injectable()
export class ControlDetailEffects {
  constructor(
    private actions$: Actions,
    private requests: ControlDetailRequests
  ) { }

  GetControlDetail$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ControlDetailActionTypes.GET),
      mergeMap(({ payload }: GetControlDetail) =>
        this.requests.GetControlDetail(payload).pipe(
          map((resp) => new GetControlDetailSuccess(resp)),
          // map((resp: ControlDetailSuccessPayload) => new GetControlDetailSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetControlDetailFailure(error)))))));

  GetControlDetailFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ControlDetailActionTypes.GET_FAILURE),
      map(({ payload }: GetControlDetailFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get control details: ${msg || payload.error}`
        });
    })));

}
