import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  RunlistActionTypes,
  GetRunlists,
  GetRunlistsSuccess,
  GetRunlistsFailure
} from './runlists.action';

import { RunlistRequests } from './runlists.requests';

@Injectable()
export class RunlistEffects {
  constructor(
    private actions$: Actions,
    private requests: RunlistRequests
  ) { }

  getRunlists$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RunlistActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id, name, id } }: GetRunlists) =>
        this.requests.getRunlist(server_id, org_id, name, id).pipe(
          map((resp) => new GetRunlistsSuccess(resp)),
          catchError((error: HttpErrorResponse) => observableOf(new GetRunlistsFailure(error)))))));

  getRunlistsFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(RunlistActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetRunlistsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get run list: ${msg || payload.error}`
        });
    })));
}
