import { catchError, map, mergeMap } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';

import {
  GetContentItemsSuccess,
  GetContentItemsFailure,
  CdsActionTypes
} from './cds.actions';
import { CdsRequests } from './cds.requests';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

@Injectable()
export class CdsEffects {
  constructor(
    private actions$: Actions,
    private requests: CdsRequests
  ) { }

  @Effect()
  getContentItems$ = this.actions$.pipe(
    ofType(CdsActionTypes.GET_CONTENT_ITEMS),
    mergeMap( (_action) =>
      this.requests.getContentItems().pipe(
        map(contentItems => new GetContentItemsSuccess( contentItems )),
        catchError((error) => of(new GetContentItemsFailure(error))))
    ));

  @Effect()
  getContentItemsFailure$ = this.actions$.pipe(
    ofType(CdsActionTypes.GET_CONTENT_ITEMS_FAILURE),
    map(({ payload: { error } }: GetContentItemsFailure) => {
      const msg = error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get content items errors: ${msg || error}`
      });
    }));
}
