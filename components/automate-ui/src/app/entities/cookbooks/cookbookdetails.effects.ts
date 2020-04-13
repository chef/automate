import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetCookbookDetailsForVersion,
  GetCookbookDetailsSuccess,
  GetCookbookDetailsFailure,
  CookbookDetailsActionTypes
} from './cookbookdetails.actions';

import {
  CookbookDetailsRequests
} from './cookbookdetails.requests';

@Injectable()
export class CookbookDetailsEffects {
  constructor(
    private actions$: Actions,
    private requests: CookbookDetailsRequests
  ) { }

  @Effect()
  getCookbooksForOrgs$ = this.actions$.pipe(
    ofType(CookbookDetailsActionTypes.GET),
    mergeMap(({ payload: { server_id, org_id, cookbook_name, cookbook_version } }
      : GetCookbookDetailsForVersion) => this.requests
        .getCookbookDetailsForVersion(server_id, org_id, cookbook_name, cookbook_version)
        .pipe(
          map((resp) => new GetCookbookDetailsSuccess(resp)),
          catchError((error: HttpErrorResponse) =>
            observableOf(new GetCookbookDetailsFailure(error))
          ))));

  @Effect()
  getCookbooksFailure$ = this.actions$.pipe(
    ofType(CookbookDetailsActionTypes.GET_FAILURE),
    map(({ payload }: GetCookbookDetailsFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get cookbook details: ${msg || payload.error}`
      });
    }));

}
