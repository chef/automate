import { EntityState, EntityAdapter, createEntityAdapter } from '@ngrx/entity';
import { set, pipe } from 'lodash/fp';
import { EntityStatus } from 'app/entities/entities';
import { NotificationRuleActionTypes, NotificationRuleActions } from './notification_rule.action';
import { NotificationRule } from './notification_rule.model';

export interface NotificationRuleEntityState extends EntityState<NotificationRule> {
  rulesStatus:  EntityStatus;
  getAllStatus: EntityStatus;
  getStatus: EntityStatus;
  updateStatus: EntityStatus;
}

const GET_ALL_STATUS = 'getAllStatus';
const GET_STATUS = 'getStatus';
const UPDATE_STATUS = 'updateStatus';

export const notificationRuleEntityAdapter:
  EntityAdapter<NotificationRule> = createEntityAdapter<NotificationRule>({
    selectId: (rule: NotificationRule) => rule.name
});

export const NotificationRuleEntityInitialState: NotificationRuleEntityState =
  notificationRuleEntityAdapter.getInitialState(<NotificationRuleEntityState>{
    getAllStatus: EntityStatus.notLoaded,
    getStatus: EntityStatus.notLoaded,
    updateStatus: EntityStatus.notLoaded,
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

    case NotificationRuleActionTypes.GET:
      return set(GET_STATUS, EntityStatus.loading,
        notificationRuleEntityAdapter.removeAll(state)
      ) as NotificationRuleEntityState;

    case NotificationRuleActionTypes.GET_SUCCESS:
      return set(GET_STATUS, EntityStatus.loadingSuccess,
        notificationRuleEntityAdapter.addOne(action.payload, state)
      ) as NotificationRuleEntityState;

    case NotificationRuleActionTypes.GET_FAILURE:
      return set( GET_STATUS, EntityStatus.loadingFailure,
        state ) as NotificationRuleEntityState;

    case NotificationRuleActionTypes.UPDATE:
      return set(UPDATE_STATUS, EntityStatus.loading, state);

    case NotificationRuleActionTypes.UPDATE_SUCCESS:
      return set(UPDATE_STATUS, EntityStatus.loadingSuccess,
        notificationRuleEntityAdapter.updateOne({
          id: action.payload.id,
          changes: action.payload
        }, state));

    case NotificationRuleActionTypes.UPDATE_FAILURE:
      return set(UPDATE_STATUS, EntityStatus.loadingFailure, state);

    default:
      return state;
  }
}
