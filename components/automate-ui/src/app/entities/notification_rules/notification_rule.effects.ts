import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map } from 'rxjs/operators';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  DeleteNotificationRule,
  DeleteNotificationRuleSuccess,
  DeleteNotificationRuleFailure,
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
  deleteNotificationRule$ = this.actions$.pipe(
      ofType(NotificationRuleActionTypes.DELETE),
      mergeMap(({ payload: { rule } }: DeleteNotificationRule) =>
        this.requests.deleteNotificationRule(rule).pipe(
          map(() => new DeleteNotificationRuleSuccess({rule})),
          catchError((error: HttpErrorResponse) =>
            observableOf(new DeleteNotificationRuleFailure(error))))));

  @Effect()
  deleteNotificationRuleSuccess$ = this.actions$.pipe(
      ofType(NotificationRuleActionTypes.DELETE_SUCCESS),
      map(({ payload: { rule } }: DeleteNotificationRuleSuccess) => {
        return new CreateNotification({
          type: Type.info,
          message: `Deleted rule ${rule.name}.`
        });
      }));

  @Effect()
  deleteNotificationRuleFailure$ = this.actions$.pipe(
      ofType(NotificationRuleActionTypes.DELETE_FAILURE),
      map(({ payload }: DeleteNotificationRuleFailure) => {
        const msg = payload.error.error;
        return new CreateNotification({
          type: Type.error,
          message: `Could not delete rule: ${msg || payload.error}`
        });
      }));

}
