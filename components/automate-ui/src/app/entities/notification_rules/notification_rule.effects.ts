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
  NotificationRuleActionTypes,
  GetNotification,
  GetNotificationSuccess,
  GetNotificationFailure,
  UpdateNotification,
  UpdateNotificationSuccess,
  UpdateNotificationFailure,
  TestNotification,
  TestNotificationSuccess,
  TestNotificationFailure
} from './notification_rule.action';
import { NotificationRuleRequests } from './notification_rule.requests';
import { NotificationRule } from './notification_rule.model';

@Injectable()
export class NotificationRuleEffects {
  constructor(
    private actions$: Actions,
    private requests: NotificationRuleRequests
  ) { }

  @Effect()
  getNotificatoins$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_ALL),
    mergeMap(() =>
      this.requests.getNotificationRules().pipe(
        map(resp => new GetNotificationRulesSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetNotificationRulesFailure(error))))));

  @Effect()
  getNotificatoinsFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetNotificationRulesFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get notifications: ${msg}`
      });
    }));

  @Effect()
  getNotificatoin$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET),
    mergeMap(({ payload: { id }}: GetNotification) =>
      this.requests.getNotificationRule(id).pipe(
        map((resp: NotificationRule) => new GetNotificationSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
        observableOf(new GetNotificationFailure(error, id))))));

  @Effect()
  getNotificatoinFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_FAILURE),
    map(({ payload, id }: GetNotificationFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get notification ${id}: ${msg || payload.error}`
      });
    }));

  @Effect()
  updateNotificatoin$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE),
    mergeMap(({ payload: { notification, username, password } }: UpdateNotification) =>
      this.requests.updateNotificationRule(notification, username, password).pipe(
        map(() => new UpdateNotificationSuccess(notification)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateNotificationFailure(error)))
      )));

  @Effect()
  updateNotificatoinSuccess$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE_SUCCESS),
    map(({ payload  }: UpdateNotificationSuccess) => new CreateNotification({
    type: Type.info,
    message: `Updated notification ${payload.name}.`
  })));

  @Effect()
  updateNotificatoinFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateNotificationFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update notification: ${msg || payload.error}.`
      });
    }));

  @Effect()
  testNotificatoin$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST),
    mergeMap(({ payload: { name, targetUrl, targetSecretID } }: TestNotification) =>
      this.requests.testNotification(targetUrl, targetSecretID).pipe(
        map(() => new TestNotificationSuccess({ name })),
        catchError(() =>
          observableOf(new TestNotificationFailure( { name }))))));

  @Effect()
  testNotificatoinSuccess$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST_SUCCESS),
    map(({ payload: { name }  }: TestNotificationSuccess) => new CreateNotification({
    type: Type.info,
    message: `Notification test connected successfully for ${name}.`
  })));

  @Effect()
  testNotificatoinFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST_FAILURE),
    map(({ payload: { name } }: TestNotificationFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Unable to connect to notification ${name}.`
      });
    }));

}
