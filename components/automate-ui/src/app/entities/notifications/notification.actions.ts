import { Action } from '@ngrx/store';

import { Type } from './notification.model';

export enum NotificationActionTypes {
  CREATE = 'NOTIFICATION::CREATE',
  DELETE = 'NOTIFICATION::DELETE'
}

export class CreateNotification implements Action {
  readonly type = NotificationActionTypes.CREATE;

  constructor(public payload: {type: Type, message: string, timeout?: number}) {}
}

export class DeleteNotification implements Action {
  readonly type = NotificationActionTypes.DELETE;

  constructor(public payload: {id: string}) {}
}

export type NotificationActions =
  | CreateNotification
  | DeleteNotification;
