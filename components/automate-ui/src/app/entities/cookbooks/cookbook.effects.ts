import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetCookbooks,
  GetCookbooksSuccess,
  CookbooksSuccessPayload,
  GetCookbooksFailure,
  CookbookActionTypes
} from './cookbook.actions';

import {
  CookbookRequests
} from './cookbook.requests';

@Injectable()
export class CookbookEffects {
  constructor(
    private actions$: Actions,
    private requests: CookbookRequests
  ) { }

  getCookbooks$ = createEffect(() =>
    this.actions$.pipe(
      ofType(CookbookActionTypes.GET_ALL),
      mergeMap(({ payload: { server_id, org_id } }: GetCookbooks) =>
        this.requests.getCookbooks(server_id, org_id).pipe(
          map((resp: CookbooksSuccessPayload) => new GetCookbooksSuccess(resp)),
          catchError(
            (error: HttpErrorResponse) => observableOf(new GetCookbooksFailure(error)
            ))))));

  getCookbooksFailure$ = createEffect(() =>
    this.actions$.pipe(
      ofType(CookbookActionTypes.GET_ALL_FAILURE),
      map(({ payload }: GetCookbooksFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get cookbooks: ${msg || payload.error}`
        });
      })));

}
