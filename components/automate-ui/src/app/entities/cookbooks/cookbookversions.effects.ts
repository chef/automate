import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetCookbookVersions,
  GetCookbookVersionsSuccess,
  GetCookbookVersionsFailure,
  CookbookVersionsActionTypes
} from './cookbookversions.actions';

import {
  CookbookVersionsRequests
} from './cookbookversions.requests';

@Injectable()
export class CookbookVersionsEffects {
  constructor(
    private actions$: Actions,
    private requests: CookbookVersionsRequests
  ) { }

  @Effect()
  getCookbookVersions$ = this.actions$.pipe(
      ofType(CookbookVersionsActionTypes.GET),
      mergeMap(({ payload: { server_id, org_id, cookbook_name } }: GetCookbookVersions) =>
        this.requests.getCookbookVersions(server_id, org_id, cookbook_name).pipe(
          map((resp) => new GetCookbookVersionsSuccess(resp)),
          catchError(
            (error: HttpErrorResponse) => observableOf(new GetCookbookVersionsFailure(error)
            )))));

  @Effect()
  getCookbookVersionsFailure$ = this.actions$.pipe(
      ofType(CookbookVersionsActionTypes.GET_FAILURE),
      map(({ payload }: GetCookbookVersionsFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not get cookbook versions: ${msg || payload.error}`
        });
      }));

}
