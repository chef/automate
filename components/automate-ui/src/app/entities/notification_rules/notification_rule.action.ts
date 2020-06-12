import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { NotificationRule } from './notification_rule.model';

export enum NotificationRuleActionTypes {
  GET_ALL = 'NOTIFICATION_RULES::GET_ALL',
  GET_ALL_SUCCESS   = 'NOTIFICATION_RULES::GET_ALL::SUCCESS',
  GET_ALL_FAILURE   = 'NOTIFICATION_RULES::GET_ALL::FAILURE',
  GET               = 'NOTIFICATION_RULES::GET',
  GET_SUCCESS       = 'NOTIFICATION_RULES::GET::SUCCESS',
  GET_FAILURE       = 'NOTIFICATION_RULES::GET::FAILURE',
  UPDATE            = 'NOTIFICATION_RULES::UPDATE',
  UPDATE_SUCCESS    = 'NOTIFICATION_RULES::UPDATE::SUCCESS',
  UPDATE_FAILURE    = 'NOTIFICATION_RULES::UPDATE::FAILURE',
  SEND_TEST         = 'NOTIFICATION_RULES::SEND_TEST',
  SEND_TEST_SUCCESS = 'NOTIFICATION_RULES::SEND_TEST::SUCCESS',
  SEND_TEST_FAILURE = 'NOTIFICATION_RULES::SEND_TEST::FAILURE'
}

export interface NotificationRuleSuccessPayload {
  rules: NotificationRule[];
}

export class GetNotificationRules implements Action {
  readonly type = NotificationRuleActionTypes.GET_ALL;
}

export class GetNotificationRulesSuccess implements Action {
  readonly type = NotificationRuleActionTypes.GET_ALL_SUCCESS;
  constructor(public payload: NotificationRule[]) { }
}

export class GetNotificationRulesFailure implements Action {
  readonly type = NotificationRuleActionTypes.GET_ALL_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class GetNotification implements Action {
  readonly type = NotificationRuleActionTypes.GET;
  constructor(public payload: { id: string }) { }
}

export class GetNotificationSuccess implements Action {
  readonly type = NotificationRuleActionTypes.GET_SUCCESS;
  constructor(public payload: NotificationRule) { }
}

export class GetNotificationFailure implements Action {
  readonly type = NotificationRuleActionTypes.GET_FAILURE;
  constructor(public payload: HttpErrorResponse, public id: string) { }
}

export class UpdateNotification implements Action {
  readonly type = NotificationRuleActionTypes.UPDATE;
  constructor(public payload: {
    notification: NotificationRule, username: string, password: string
  }) { }
}

export class UpdateNotificationSuccess implements Action {
  readonly type = NotificationRuleActionTypes.UPDATE_SUCCESS;
  constructor(public payload) { }
}

export class UpdateNotificationFailure implements Action {
  readonly type = NotificationRuleActionTypes.UPDATE_FAILURE;
  constructor(public payload: HttpErrorResponse) { }
}

export class TestNotification implements Action {
  readonly type = NotificationRuleActionTypes.SEND_TEST;
  constructor(public payload: { name: string, targetUrl: string, targetSecretID: string }) { }
}

export class TestNotificationSuccess implements Action {
  readonly type = NotificationRuleActionTypes.SEND_TEST_SUCCESS;
  constructor(public payload: { name: string }) { }
}

export class TestNotificationFailure implements Action {
  readonly type = NotificationRuleActionTypes.SEND_TEST_FAILURE;
  constructor(public payload: { name: string }) { }
}


export type NotificationRuleActions =
  | GetNotificationRules
  | GetNotificationRulesSuccess
  | GetNotificationRulesFailure
  | GetNotification
  | GetNotificationSuccess
  | GetNotificationFailure
  | UpdateNotification
  | UpdateNotificationSuccess
  | UpdateNotificationFailure
  | TestNotification
  | TestNotificationSuccess
  | TestNotificationFailure;
