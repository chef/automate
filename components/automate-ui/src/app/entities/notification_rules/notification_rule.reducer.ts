import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { NotificationRuleActionTypes, NotificationRuleActions } from './notification_rule.action';
import { NotificationRule } from './notification_rule.model';

export interface NotificationRuleEntityState extends EntityState<NotificationRule> {
  rulesStatus:  EntityStatus;
  getAllStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';

export const notificationRuleEntityAdapter:
  EntityAdapter<NotificationRule> = createEntityAdapter<NotificationRule>({
    selectId: (rule: NotificationRule) => rule.name
});

export const NotificationRuleEntityInitialState: NotificationRuleEntityState =
  notificationRuleEntityAdapter.getInitialState(<NotificationRuleEntityState>{
    getAllStatus: EntityStatus.notLoaded
  });

export function notificationRuleEntityReducer(
  state: NotificationRuleEntityState = NotificationRuleEntityInitialState,
  action: NotificationRuleActions): NotificationRuleEntityState {

  switch (action.type) {
    case NotificationRuleActionTypes.GET_ALL:
      return set(GET_ALL_STATUS, EntityStatus.loading,
        notificationRuleEntityAdapter.removeAll(state));

    case NotificationRuleActionTypes.GET_ALL_SUCCESS:
      return pipe(
        set(GET_ALL_STATUS, EntityStatus.loadingSuccess))
        (notificationRuleEntityAdapter.setAll(action.payload, state));

    case NotificationRuleActionTypes.GET_ALL_FAILURE:
      return set(GET_ALL_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
