import { defaults, concat, reject } from 'lodash/fp';
import { Notification, Type } from './notification.model';
import { NotificationActions, NotificationActionTypes } from './notification.actions';

let uid = 0;

export type NotificationEntityState = Notification[];

export const InitialState: NotificationEntityState = [];

export function notificationEntityReducer(
  state: NotificationEntityState = InitialState,
  action: NotificationActions): NotificationEntityState {

  switch (action.type) {

    case NotificationActionTypes.CREATE:
      const timeout = action.payload['type'] === Type.info ? 3 : 30;

      const notification = defaults({
        id: (uid++).toString(),
        type: Type.error,
        timeout: timeout
      }, action.payload);

      return concat(notification, state);

    case NotificationActionTypes.DELETE:
      return reject(['id', action.payload.id], state);

    default:
      return state;

  }
}
