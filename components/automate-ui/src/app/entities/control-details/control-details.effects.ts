import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  ControlDetailsActionTypes,
  GetControlDetails,
  GetControlDetailsSuccess,
  GetControlDetailsFailure,
  ControlDetailsSuccessPayload
} from './control-details.action';

import { ControlDetailsRequests } from './control-details.requests';

@Injectable()
export class ControlDetailsEffects {
  constructor(
    private actions$: Actions,
    private requests: ControlDetailsRequests
  ) { }

  GetControlDetails$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ControlDetailsActionTypes.GET),
      mergeMap(({ payload: { filters } }: GetControlDetails) =>
        this.requests.GetControlDetails(filters).pipe(
          // map((resp) => new GetControlDetailsSuccess(Response)),
          map((resp: ControlDetailsSuccessPayload) => new GetControlDetailsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
          observableOf(new GetControlDetailsFailure(error)))))));

  GetControlDetailsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(ControlDetailsActionTypes.GET_FAILURE),
      map(({ payload }: GetControlDetailsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get control details: ${msg || payload.error}`
        });
    })));

}
