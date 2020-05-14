import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set } from 'lodash/fp';

import { EntityStatus } from 'app/entities/entities';
import { NotificationRuleActionTypes, NotificationRuleActions } from './notification_rule.action';
import { Rule } from 'app/pages/notifications/rule';

export interface NotificationRuleEntityState extends EntityState<Rule> {
  deleteStatus: EntityStatus;
}

const DELETE_STATUS = 'deleteStatus';

export const notificationRuleEntityAdapter: EntityAdapter<Rule> = createEntityAdapter<Rule>();

export const NotificationRuleEntityInitialState: NotificationRuleEntityState =
notificationRuleEntityAdapter.getInitialState(<NotificationRuleEntityState>{
    deleteStatus: EntityStatus.notLoaded,
  });

export function notificationRuleEntityReducer(
  state: NotificationRuleEntityState = NotificationRuleEntityInitialState,
  action: NotificationRuleActions): NotificationRuleEntityState {

  switch (action.type) {
    case NotificationRuleActionTypes.DELETE:
      return set(DELETE_STATUS, EntityStatus.loading, state);

    case NotificationRuleActionTypes.DELETE_SUCCESS:
      return set(DELETE_STATUS, EntityStatus.loadingSuccess,
        notificationRuleEntityAdapter.removeOne(action.payload.rule.id, state));

    case NotificationRuleActionTypes.DELETE_FAILURE:
      return set(DELETE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
