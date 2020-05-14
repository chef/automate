import { HttpErrorResponse } from '@angular/common/http';
import { Action } from '@ngrx/store';
import { Rule } from 'app/pages/notifications/rule';

export enum NotificationRuleActionTypes {
  DELETE          = 'NOTIFICATION_RULES::DELETE',
  DELETE_SUCCESS  = 'NOTIFICATION_RULES::DELETE::SUCCESS',
  DELETE_FAILURE  = 'NOTIFICATION_RULES::DELETE::FAILURE',
}

export class DeleteNotificationRule implements Action {
  readonly type = NotificationRuleActionTypes.DELETE;

  constructor(public payload: { rule: Rule }) { }
}

export class DeleteNotificationRuleSuccess implements Action {
  readonly type = NotificationRuleActionTypes.DELETE_SUCCESS;

  constructor(public payload: { rule: Rule }) { }
}

export class DeleteNotificationRuleFailure implements Action {
  readonly type = NotificationRuleActionTypes.DELETE_FAILURE;

  constructor(public payload: HttpErrorResponse) { }
}

export type NotificationRuleActions =
  | DeleteNotificationRule
  | DeleteNotificationRuleSuccess
  | DeleteNotificationRuleFailure;
