import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  GetNotificationRulesSuccess,
  GetNotificationRulesFailure,
  NotificationRuleActionTypes
} from './notification_rule.action';

import { NotificationRuleRequests } from './notification_rule.requests';

@Injectable()
export class NotificationRuleEffects {
  constructor(
    private actions$: Actions,
    private requests: NotificationRuleRequests
  ) { }

  @Effect()
  getDestinations$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_ALL),
    mergeMap(() =>
      this.requests.getNotificationRules().pipe(
        map(resp => new GetNotificationRulesSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetNotificationRulesFailure(error))))));

  @Effect()
  getDestinationsFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetNotificationRulesFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get notifications: ${msg || payload.error}`
      });
    }));

}
