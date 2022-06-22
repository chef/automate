import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';

import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  DataFeedGlobalConfigActionTypes,
  GlobalDataFeedConfigSuccess,
  GlobalDataFeedConfigFailure
} from './destination-config.actions';

import {
  DataFeedGlobalConfigRequests
} from './destination-config.requests';

@Injectable()
export class DestinationGlobalConfigEffects {
  constructor(
    private actions$: Actions,
    private requests: DataFeedGlobalConfigRequests
  ) { }

    globalDataFeedConfig$ = createEffect(() =>
      this.actions$.pipe(
        ofType(DataFeedGlobalConfigActionTypes.GLOBAL_CONFIG),
        mergeMap(() =>
          this.requests.globalDataFeedConfig().pipe(
            map((resp: GlobalDataFeedConfigSuccess) => new GlobalDataFeedConfigSuccess(resp)),
            catchError((error: HttpErrorResponse) =>
              observableOf(new GlobalDataFeedConfigFailure(error)))))));

    globalDataFeedConfigFailure$ = createEffect(() =>
      this.actions$.pipe(
        ofType(DataFeedGlobalConfigActionTypes.GLOBAL_CONFIG_FAILURE),
        map(({ payload  }: GlobalDataFeedConfigFailure) => new CreateNotification({
          type: Type.error,
          message: `Error while fetching data-feed config: error ${payload.error}.`
        }))));

}
