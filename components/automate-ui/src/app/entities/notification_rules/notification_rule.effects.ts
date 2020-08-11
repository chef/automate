import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, Effect, ofType } from '@ngrx/effects';
import { of as observableOf } from 'rxjs';
import { catchError, mergeMap, map, filter } from 'rxjs/operators';
import { HttpStatus } from 'app/types/types';
import { CreateNotification } from 'app/entities/notifications/notification.actions';
import { Type } from 'app/entities/notifications/notification.model';

import {
  CreateNotificationRule,
  CreateNotificationRuleSuccess,
  CreateNotificationRuleFailure,
  GetNotificationRulesSuccess,
  GetNotificationRulesFailure,
  GetNotification,
  GetNotificationSuccess,
  GetNotificationFailure,
  UpdateNotification,
  UpdateNotificationSuccess,
  UpdateNotificationFailure,
  TestNotification,
  TestNotificationSuccess,
  TestNotificationFailure,
  DeleteNotificationRule,
  DeleteNotificationRuleSuccess,
  DeleteNotificationRuleFailure,
  NotificationRuleActionTypes
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

  @Effect()
  createNotificationRule$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.CREATE),
    mergeMap(({ payload, username, password }: CreateNotificationRule) =>
    this.requests.createNotificationRule( payload, username, password ).pipe(
      map((resp) => new CreateNotificationRuleSuccess(NotificationRule.fromResponse(resp.rule))),
      catchError((error: HttpErrorResponse) =>
        observableOf(new CreateNotificationRuleFailure(error))))));

  @Effect()
  createNotificationRuleSuccess$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.CREATE_SUCCESS),
    map(({ payload  }: CreateNotificationRuleSuccess) => new CreateNotification({
    type: Type.info,
    message: `Created notification ${payload.name}.`
  })));

  @Effect()
  createNotificationRuleFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateNotificationRuleFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateNotificationRuleFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create notification: ${payload.error.error || payload}.`
      })));

  @Effect()
  deleteNotificationRule$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.DELETE),
    mergeMap(({ payload: { id, name } }: DeleteNotificationRule) =>
      this.requests.deleteNotificationRule(id).pipe(
        map(() => new DeleteNotificationRuleSuccess({id, name})),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteNotificationRuleFailure(error))))));

  @Effect()
  deleteNotificationRuleSuccess$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }: DeleteNotificationRuleSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Deleted rule ${name}.`
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

  @Effect()
  getNotification$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET),
    mergeMap(({ payload: { id }}: GetNotification) =>
      this.requests.getNotificationRule(id).pipe(
        map((resp: NotificationRule) => new GetNotificationSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
        observableOf(new GetNotificationFailure(error, id))))));

  @Effect()
  getNotificationFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_FAILURE),
    map(({ payload, id }: GetNotificationFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not get notification ${id}: ${msg || payload.error}`
      });
    }));

  @Effect()
  updateNotification$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE),
    mergeMap(({ payload: { notification } }: UpdateNotification) =>
      this.requests.updateNotificationRule(notification).pipe(
        map(() => new UpdateNotificationSuccess(notification)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateNotificationFailure(error)))
      )));

  @Effect()
  updateNotificationSuccess$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE_SUCCESS),
    map(({ payload  }: UpdateNotificationSuccess) => new CreateNotification({
    type: Type.info,
    message: `Updated notification ${payload.name}.`
  })));

  @Effect()
  updateNotificationFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateNotificationFailure) => {
      const msg = payload.error.error;
      return new CreateNotification({
        type: Type.error,
        message: `Could not update notification: ${msg || payload.error}.`
      });
    }));

  @Effect()
  testNotification$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST),
    mergeMap(({ payload: { name, targetUrl, targetSecretID } }: TestNotification) =>
      this.requests.testNotification(targetUrl, targetSecretID).pipe(
        map(() => new TestNotificationSuccess({ name })),
        catchError(() =>
          observableOf(new TestNotificationFailure( { name }))))));

  @Effect()
  testNotificationSuccess$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST_SUCCESS),
    map(({ payload: { name }  }: TestNotificationSuccess) => new CreateNotification({
    type: Type.info,
    message: `Notification test connected successfully for ${name}.`
  })));

  @Effect()
  testNotificationFailure$ = this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST_FAILURE),
    map(({ payload: { name } }: TestNotificationFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Unable to connect to notification ${name}.`
      });
    }));
}
