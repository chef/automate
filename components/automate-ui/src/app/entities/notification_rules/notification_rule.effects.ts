import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
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

  getNotifications$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_ALL),
    mergeMap(() =>
      this.requests.getNotificationRules().pipe(
        map(resp => new GetNotificationRulesSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new GetNotificationRulesFailure(error)))))));

  getNotificationsFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_ALL_FAILURE),
    map(({ payload }: GetNotificationRulesFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could not get notifications: ${payload.error.error || payload.error}`
      });
    })));

  createNotificationRule$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.CREATE),
    mergeMap(({ payload, username, password }: CreateNotificationRule) =>
    this.requests.createNotificationRule( payload, username, password ).pipe(
      map((resp) => new CreateNotificationRuleSuccess(NotificationRule.fromResponse(resp.rule))),
      catchError((error: HttpErrorResponse) =>
        observableOf(new CreateNotificationRuleFailure(error)))))));

  createNotificationRuleSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NotificationRuleActionTypes.CREATE_SUCCESS),
      map(({ payload  }: CreateNotificationRuleSuccess) => new CreateNotification({
      type: Type.info,
      message: `Created notification ${payload.name}.`
    }))));

  createNotificationRuleFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.CREATE_FAILURE),
    filter(({ payload }: CreateNotificationRuleFailure) => payload.status !== HttpStatus.CONFLICT),
    map(({ payload }: CreateNotificationRuleFailure) => new CreateNotification({
        type: Type.error,
        message: `Could not create notification: ${payload.error.error || payload}.`
      }))));

  deleteNotificationRule$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.DELETE),
    mergeMap(({ payload: { id, name } }: DeleteNotificationRule) =>
      this.requests.deleteNotificationRule(id).pipe(
        map(() => new DeleteNotificationRuleSuccess({id, name})),
        catchError((error: HttpErrorResponse) =>
          observableOf(new DeleteNotificationRuleFailure(error)))))));

  deleteNotificationRuleSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.DELETE_SUCCESS),
    map(({ payload: { name } }: DeleteNotificationRuleSuccess) => {
      return new CreateNotification({
        type: Type.info,
        message: `Deleted rule ${name}.`
      });
    })));

  deleteNotificationRuleFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.DELETE_FAILURE),
    map(({ payload }: DeleteNotificationRuleFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could not delete rule: ${payload.error.error || payload.error}`
      });
    })));

  getNotification$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET),
    mergeMap(({ payload: { id }}: GetNotification) =>
      this.requests.getNotificationRule(id).pipe(
        map((resp: NotificationRule) => new GetNotificationSuccess(resp)),
        catchError((error: HttpErrorResponse) =>
        observableOf(new GetNotificationFailure(error, id)))))));

  getNotificationFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.GET_FAILURE),
    map(({ payload, id }: GetNotificationFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could not get notification ${id}: ${payload.error.error || payload.error}`
      });
    })));

  updateNotification$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE),
    mergeMap(({ payload: { notification } }: UpdateNotification) =>
      this.requests.updateNotificationRule(notification).pipe(
        map(() => new UpdateNotificationSuccess(notification)),
        catchError((error: HttpErrorResponse) =>
          observableOf(new UpdateNotificationFailure(error)))
      ))));

  updateNotificationSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NotificationRuleActionTypes.UPDATE_SUCCESS),
      map(({ payload  }: UpdateNotificationSuccess) => new CreateNotification({
      type: Type.info,
      message: `Updated notification ${payload.name}.`
    }))));

  updateNotificationFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.UPDATE_FAILURE),
    map(({ payload }: UpdateNotificationFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Could not update notification: ${payload.error.error || payload.error}.`
      });
    })));

  testNotification$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST),
    mergeMap(({ payload: { name, targetUrl, targetSecretID } }: TestNotification) =>
      this.requests.testNotification(targetUrl, targetSecretID).pipe(
        map(() => new TestNotificationSuccess({ name })),
        catchError(() =>
          observableOf(new TestNotificationFailure( { name })))))));

  testNotificationSuccess$ = createEffect(() =>
    this.actions$.pipe(
      ofType(NotificationRuleActionTypes.SEND_TEST_SUCCESS),
      map(({ payload: { name }  }: TestNotificationSuccess) => new CreateNotification({
      type: Type.info,
      message: `Notification test connected successfully for ${name}.`
    }))));

  testNotificationFailure$ = createEffect(() =>
    this.actions$.pipe(
    ofType(NotificationRuleActionTypes.SEND_TEST_FAILURE),
    map(({ payload: { name } }: TestNotificationFailure) => {
      return new CreateNotification({
        type: Type.error,
        message: `Unable to connect to notification ${name}.`
      });
    })));

}
